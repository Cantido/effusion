defmodule Effusion.Connections do
  use GenServer, restart: :temporary
  alias Effusion.Messages
  alias Effusion.TCPSocket
  alias Effusion.Connections.Connection
  alias Effusion.Torrents
  alias Effusion.Swarm
  require Logger

  @behaviour :ranch_protocol

  @moduledoc """
  Protocol handler for the Peer Wire Protocol.

  This amounts to performing the handshake, and then forwarding any messages
  to the associated download.
  """

  def connect(address, info_hash) do
    DynamicSupervisor.start_child(
      Effusion.ConnectionSupervisor,
      {__MODULE__, [info_hash: info_hash, address: address]}
    )
  end

  @doc """
  The `start_link` implementation for `:ranch_protocol` behaviour
  """
  def start_link(ref, _socket, transport, _opts) do
    pid = :proc_lib.spawn_link(__MODULE__, :incoming_init, [ref, transport])
    {:ok, pid}
  end

  def start_link(args) do
    info_hash = Keyword.fetch!(args, :info_hash)
    address = Keyword.fetch!(args, :address)
    GenServer.start_link(__MODULE__, args, name: via(info_hash, address))
  end

  def send(info_hash, address, message) do
    GenServer.call(via(info_hash, address), {:send, message})
  end

  def broadcast(info_hash, message) do
    all_for_download(info_hash)
    |> Enum.each(fn address ->
      GenServer.call(via(info_hash, address), {:send, message})
    end)
  end

  def disconnect(info_hash, address) do
    GenServer.call(via(info_hash, address), :disconnect)
  end

  def disconnect_all(info_hash) do
    all_for_download(info_hash)
    |> Enum.each(fn address ->
      disconnect(info_hash, address)
    end)
  end

  def upload_speed(info_hash, address) do
    GenServer.call(via(info_hash, address), :upload_speed)
  end

  def download_speed(info_hash, address) do
    GenServer.call(via(info_hash, address), :download_speed)
  end

  defp via(info_hash, address) do
    {:via, Registry, {ConnectionRegistry, {info_hash, address}}}
  end

  def all_for_download(info_hash) do
    match_pattern = {{:"$1", :"$2"}, :_, :_}
    guards = [{:==, :"$1", info_hash}]
    body = [:"$2"]

    spec = [{match_pattern, guards, body}]

    Registry.select(ConnectionRegistry, spec)
  end

  def init(args) do
    info_hash = Keyword.fetch!(args, :info_hash)
    address = Keyword.fetch!(args, :address)

    state =
      %Connection{
        direction: :outgoing,
        address: address,
        info_hash: info_hash
      }

    {:ok, state, {:continue, {:connect, address}}}
  end

  def incoming_init(ref, transport) do
    {:ok, socket} = :ranch.handshake(ref)
    :ok = transport.setopts(socket, active: :once)
    {:ok, address} = :inet.peername(socket)

    :gen_server.enter_loop(__MODULE__, [], %{
      address: address,
      socket: socket,
      transport: transport,
      direction: :incoming
    })
  end

  def handle_continue({:connect, {host, port}}, conn) do
    Logger.debug("TCP worker attempting connection to peer #{inspect {host, port}}")

    our_peer_id = Application.fetch_env!(:effusion, :peer_id)
    expected_peer_id = Swarm.peer_id(conn.address)

    with {:ok, socket, _remote_peer_id, _remote_extensions} <- TCPSocket.connect({host, port}, conn.info_hash, our_peer_id, expected_peer_id, []) do
      conn = Connection.set_socket(conn, socket)
      Swarm.decrement_failcount({host, port})
      :ok = :inet.setopts(socket, active: :once)
      {:noreply, conn, {:continue, :send_bitfield}}
    else
      _ ->
        Swarm.increment_failcount({host, port})
        {:stop, :failed_to_connect, conn}
    end
  end

  def handle_continue(:send_bitfield, conn) do
    bitfield = Torrents.get_bitfield(conn.info_hash)
    bitfield_message = {:bitfield, bitfield}
    case TCPSocket.send_msg(conn.socket, bitfield_message) do
      {:ok, bytes_count} ->
        conn = Connection.add_upload_event(conn, bytes_count, DateTime.utc_now())
        {:noreply, conn}
      {:error, err} -> {:stop, conn, {:tcp_failed_to_send, err}}
    end
  end

  def handle_continue({:send, :interested}, conn) do
    if conn.am_interested do
      {:noreply, conn}
    else
      {:ok, bytes_count} = TCPSocket.send_msg(conn.socket, :interested)

      conn =
        conn
        |> Connection.interested_in_peer()
        |> Connection.add_upload_event(bytes_count, DateTime.utc_now())

      {:noreply, conn}
    end
  end

  def handle_continue({:send, message}, conn) do
    {:ok, bytes_count} = TCPSocket.send_msg(conn.socket, message)

    conn = Connection.add_upload_event(conn, bytes_count, DateTime.utc_now())

    {:noreply, conn}
  end

  def handle_call({:send, message}, _from, conn) do
    {:reply, :ok, conn, {:continue, {:send, message}}}
  end

  def handle_call(:upload_speed, _from, conn) do
    {:reply, Connection.upload_speed(conn, DateTime.utc_now()), conn}
  end

  def handle_call(:download_speed, _from, conn) do
    {:reply, Connection.download_speed(conn, DateTime.utc_now()), conn}
  end

  def handle_call(:disconnect, _from, conn) do
    {:stop, :normal, :ok, conn}
  end

  def handle_info({:tcp, _tcp_socket, data}, conn) when is_binary(data) do
    {:ok, msg} = Messages.decode(data)
    {:ok, conn} = handle_pwp_message(msg, conn)

    :ok = :inet.setopts(conn.socket, active: :once)
    {:noreply, conn}
  end

  def handle_info({:tcp_closed, _socket}, state), do: {:stop, "peer closed connection", state}

  def handle_info(_info, state) do
    {:noreply, state}
  end

  def terminate(reason, conn) do
    Logger.info("Connection closing with reason #{inspect reason}")
    unless is_nil(conn.socket) do
      TCPSocket.close(conn.socket)
    end

    :ok
  end

  def handle_pwp_message({:handshake, peer_id, info_hash, _extensions}, %{direction: :incoming} = conn) do
    with :ok = Swarm.set_peer_id(conn.address, peer_id) do
      conn =
        %Connection{
          direction: :incoming,
          address: conn.address,
          info_hash: info_hash
        }
        |> Connection.set_socket(conn.socket)
        |> Connection.add_download_event(68, DateTime.utc_now())

      our_peer_id = Application.fetch_env!(:effusion, :peer_id)
      # TODO: Validate info hash
      our_info_hash = info_hash
      handshake = {:handshake, our_peer_id, our_info_hash, []}
      {:ok, handshake_bytes_count} = TCPSocket.send_msg(conn.socket, handshake)
      :ok = :inet.setopts(conn.socket, packet: 4)
      conn = Connection.add_upload_event(conn, handshake_bytes_count, DateTime.utc_now())

      Registry.register(ConnectionRegistry, {info_hash, conn.address}, nil)

      bitfield = Torrents.get_bitfield(conn.info_hash)
      bitfield_message = {:bitfield, bitfield}
      {:ok, bitfield_bytes_count} = TCPSocket.send_msg(conn.socket, bitfield_message)
      :ok = :inet.setopts(conn.socket, active: :once)
      conn = Connection.add_upload_event(conn, bitfield_bytes_count, DateTime.utc_now())

      {:ok, conn}
    end
  end

  def handle_pwp_message({:bitfield, bitfield}, conn) do
    IntSet.new(bitfield)
    |> Enum.each(&Torrents.peer_has_piece(conn.info_hash, conn.address, &1))

    {:ok, conn}
  end

  def handle_pwp_message(:interested, conn) do
    conn = Connection.peer_interested_in_us(conn)

    {:ok, conn}
  end

  def handle_pwp_message(:uninterested, conn) do
    conn = Connection.peer_not_interested_in_us(conn)

    {:ok, conn}
  end

  def handle_pwp_message(:choke, conn) do
    conn = Connection.choke_us(conn)

    :ok = Torrents.drop_requests(conn.info_hash, conn.address)

    {:ok, conn}
  end

  def handle_pwp_message(:unchoke, conn) do
    conn = Connection.unchoke_us(conn)

    if Connection.can_download?(conn) do
      blocks = Torrents.get_block_requests(conn.info_hash, conn.address)

      Enum.each(blocks, fn {index, offset, size} ->
        request_message = {:request, index, offset, size}
        {:ok, bytes_count} = TCPSocket.send_msg(conn.socket, request_message)
        conn = Connection.add_upload_event(conn, bytes_count, DateTime.utc_now())

        :ok = Torrents.block_requested(conn.info_hash, conn.address, index, offset, size)
      end)
    end

    {:ok, conn}
  end

  def handle_pwp_message({:have, index}, conn) do
    :ok = Torrents.peer_has_piece(conn.info_hash, conn.address, index)

    {:ok, conn}
  end

  def handle_pwp_message({:piece, %{index: index, offset: offset, data: data}}, conn) do
    Logger.debug("Got piece, emitting event for it")
    Solvent.publish(
      "io.github.cantido.effusion.blocks.received",
      subject: conn.info_hash,
      data: %{
        address: conn.address,
        index: index,
        offset: offset,
        data: data
      }
    )

    {:ok, conn}
  end

  def handle_pwp_message({:request, _}, conn) do
    {:ok, conn}
  end
end
