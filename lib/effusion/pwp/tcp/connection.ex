defmodule Effusion.PWP.TCP.Connection do
  use GenServer, restart: :temporary
  alias Effusion.Application.ConnectionSupervisor
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.PWP.Messages
  alias Effusion.PWP.ProtocolHandler
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
  def connect(peer = {{_host, port}, info_hash, expected_peer_id}) when is_integer(port) and is_hash(info_hash) and is_peer_id(expected_peer_id) do
    ConnectionSupervisor.start_child(peer)
  end

  @doc """
  Break the connection with the given reason.
  """
  def disconnect(info_hash, peer_id, reason) do
    pid = ConnectionRegistry.get_pid(info_hash, peer_id)
    disconnect(pid, reason)
  end

  @doc """
  Break the connection.
  """
  def disconnect(pid) do
    send(pid, :disconnect)
  end

  @doc """
  Break the connection with the given reason.
  """
  def disconnect(pid, reason) do
    send(pid, {:disconnect, reason})
  end

  def recv_handshake(info_hash, peer_id) do
    pid = ConnectionRegistry.get_pid!(info_hash, peer_id)
    GenServer.call(pid, :recv_handshake)
  end

  def handshake_successful(info_hash, peer_id) do
    pid = ConnectionRegistry.get_pid!(info_hash, peer_id)
    GenServer.call(pid, :handshake_successful)
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
  def start_link(peer = {{_host, port}, info_hash, expected_peer_id}) when is_integer(port) and is_hash(info_hash) and is_peer_id(expected_peer_id)  do
    GenServer.start_link(__MODULE__, peer)
  end

  def init({address, info_hash, expected_peer_id}) do
    state = %{
      address: address,
      info_hash: info_hash,
      remote_peer_id: expected_peer_id
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
    with {:ok, {host, port}} <- :inet.peername(socket),
         :ok <- :inet.setopts(socket, active: :once, packet: 4) do
      :ok
    else
      err -> err
    end
  end


  def handle_continue(:connect, %{address: address = {host, port}, info_hash: info_hash, remote_peer_id: expected_peer_id} = state)
  when is_integer(port) and is_hash(info_hash) and is_peer_id(expected_peer_id) do
    with {:ok, _pid} <- ConnectionRegistry.register(info_hash, expected_peer_id),
         {:ok, socket} <- :gen_tcp.connect(host, port, [:binary, active: false, keepalive: true], 30_000),
         :ok <- Effusion.CQRS.Contexts.Peers.send_handshake(info_hash, expected_peer_id, host, port, :us) do
      {:noreply, Map.put(state, :socket, socket)}
    else
      {:error, reason} -> {:stop, reason, state}
    end
  end

  @doc """
  Handle a Peer Wire Protocol message.
  """
  def handle_btp(btp_message, state)

  def handle_btp({:handshake, remote_peer_id, info_hash, extensions}, state = %{address: {host, port}, socket: socket})
    when is_peer_id(remote_peer_id)
     and is_hash(info_hash) do


    with {:ok, _pid} = ConnectionRegistry.register(info_hash, remote_peer_id),
         :ok <- Effusion.CQRS.Contexts.Peers.add(info_hash, remote_peer_id, host, port, :connection),
         :ok <- Effusion.CQRS.Contexts.Peers.handle_handshake(info_hash, remote_peer_id, host, port, :them, extensions) do
      {:noreply, Map.merge(state, %{info_hash: info_hash, remote_peer_id: remote_peer_id})}
    else
      err -> {:stop, {:handshake_failure, err}, state}
    end
  end

  def handle_btp(msg, state = %{info_hash: info_hash, remote_peer_id: peer_id, socket: socket}) do
    {:ok, {host, port}} = :inet.peername(socket)
    :ok = Effusion.CQRS.Contexts.Downloads.handle_message(info_hash, peer_id, host, port, msg)
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

  def handle_info({:btp_send, dest_peer_id, msg}, state = %{socket: socket, remote_peer_id: peer_id})
  when dest_peer_id == peer_id do
    case Socket.send_msg(socket, msg) do
      :ok -> {:noreply, state}
      {:error, reason} -> {:stop, {:send_failure, reason}, state}
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
    :telemetry.execute(
      [:pwp, :disconnect],
      %{},
      Map.take(state, [:remote_peer_id, :info_hash])
      |> Map.put(:reason, reason)
    )

    if Map.has_key?(state, :socket) do
      Socket.close(state.socket)
    end

    Effusion.CQRS.Contexts.Peers.disconnected(
        Map.get(state, :info_hash),
        Map.get(state, :peer_id),
        Map.get(state, :address) |> elem(0),
        Map.get(state, :address) |> elem(1),
        reason
      )

    :ok
  end
end
