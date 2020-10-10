defmodule Effusion.PWP.TCP.Connection do
  use GenServer, restart: :temporary
  alias Effusion.Application.ConnectionSupervisor
  alias Effusion.PWP.Messages
  alias Effusion.PWP.TCP.Socket
  import Effusion.BTP.Peer, only: [is_peer_id: 1]
  import Effusion.Hash, only: [is_hash: 1]
  require Logger

  @behaviour :ranch_protocol

  @moduledoc """
  Protocol handler for the Peer Wire Protocol.

  This amounts to performing the handshake, and then forwarding any messages
  to the associated download.
  """

  @doc """
  Start a connection to a `peer` in the Connection supervision hierarchy.
  """
  def connect(peer = {{_host, port}, _peer_uuid}) when is_integer(port) do
    ConnectionSupervisor.start_child(peer)
  end

  def send_pwp_message(peer_uuid, message) do
    case Registry.lookup(ConnectionRegistry, peer_uuid) do
      [{conn, _}] -> GenServer.call(conn, {:btp_send, message})
      _ -> {:error, :peer_not_found}
    end
  end

  def recv_handshake(peer_uuid) do
    case Registry.lookup(ConnectionRegistry, peer_uuid) do
      [{conn, _}] -> GenServer.call(conn, :recv_handshake)
      _ -> {:error, :peer_not_found}
    end
  end

  def handshake_successful(peer_uuid) do
    case Registry.lookup(ConnectionRegistry, peer_uuid) do
      [{conn, _}] -> GenServer.call(conn, :handshake_successful)
      _ -> {:error, :peer_not_found}
    end
  end

  @doc """
  The `start_link` implementation for `:ranch_protocol` behaviour
  """
  def start_link(ref, socket, transport, _opts) do
    pid = :proc_lib.spawn_link(__MODULE__, :incoming_init, [ref, socket, transport])
    {:ok, pid}
  end

  @doc """
  Start a connection to a `peer`, and link the resulting process to the current process.
  """
  def start_link(peer = {{_host, port}, _peer_uuid}) when is_integer(port) do
    GenServer.start_link(__MODULE__, peer)
  end

  def init({address, peer_uuid}) do
    state = %{
      address: address,
      peer_uuid: peer_uuid
    }
    {:ok, state, {:continue, :connect}}
  end

  def incoming_init(ref, socket, transport) do
    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, active: :once)
    {:ok, address} = :inet.peername(socket)
    :gen_server.enter_loop(__MODULE__, [], %{address: address, socket: socket, transport: transport})
  end

  defp successful_handshake(socket) do
    :inet.setopts(socket, active: :once, packet: 4)
  end


  def handle_continue(:connect, %{address: {host, port}, peer_uuid: peer_uuid} = state) do
    with {:ok, _pid} <- Registry.register(ConnectionRegistry, peer_uuid, nil),
         {:ok, socket} <- :gen_tcp.connect(host, port, [:binary, active: false, keepalive: true], 30_000),
         :ok <- Effusion.CQRS.Contexts.Peers.add_opened_peer_connection(peer_uuid, host, port) do
      {:noreply, Map.put(state, :socket, socket)}
    else
      _ -> Effusion.CQRS.Contexts.Peers.handle_failed_connection_attempt(peer_uuid)
    end
  end

  @doc """
  Handle a Peer Wire Protocol message.
  """
  def handle_btp(btp_message, state)

  def handle_btp(message = {:handshake, remote_peer_id, info_hash, _extensions}, state = %{address: {host, port}})
    when is_peer_id(remote_peer_id)
     and is_hash(info_hash) do
    peer_uuid = UUID.uuid4()
    {:ok, _pid} = Registry.register(ConnectionRegistry, peer_uuid, nil)
    :ok = Effusion.CQRS.Contexts.Peers.add(peer_uuid, info_hash, remote_peer_id, host, port, :connection)
    :ok = Effusion.CQRS.Contexts.Peers.handle_message(peer_uuid, message, :them)
    {:noreply, Map.merge(state, %{info_hash: info_hash, remote_peer_id: remote_peer_id, peer_uuid: peer_uuid})}
  end

  def handle_btp(msg, state = %{peer_uuid: peer_uuid}) do
    :ok = Effusion.CQRS.Contexts.Peers.handle_message(peer_uuid, msg)
    {:noreply, state}
  end

  def handle_btp(msg, state) do
    Logger.error("Connection not able to handle message #{inspect msg}.")
    {:stop, :unexpected_message, state}
  end

  def handle_call(:recv_handshake, _from, %{socket: socket} = state) do
    handshake = Socket.recv(socket, 68)

    {:reply, handshake, state}
  end

  def handle_call(:handshake_successful, _from, %{socket: socket} = state) do
    successful_handshake(socket)

    {:reply, :ok, state}
  end

  def handle_call({:btp_send, msg}, _from, state = %{socket: socket}) do
    Logger.debug("********** Sending message #{inspect msg}")
    case Socket.send_msg(socket, msg) do
      :ok -> {:reply, :ok, state}
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  def handle_info({:tcp, _tcp_socket, data}, state) when is_binary(data) do
    {:ok, msg} = Messages.decode(data)
    ret = handle_btp(msg, state)
    :ok = :inet.setopts(state.socket, active: :once)
    ret
  end

  def handle_info({:tcp_closed, _socket}, state), do: {:stop, :normal, state}
  def handle_info(:disconnect, state), do: {:stop, :normal, state}
  def handle_info({:disconnect, reason}, state), do: {:stop, reason, state}

  def handle_info(info, state) do
    Logger.warn("Handler received unknown message: #{inspect(info)}")
    {:noreply, state}
  end

  def terminate(reason, state) when is_map(state) do
    if Map.has_key?(state, :socket) do
      Socket.close(state.socket)
    end

    Effusion.CQRS.Contexts.Peers.disconnected(
        Map.get(state, :peer_uuid),
        reason
      )

    :ok
  end
end
