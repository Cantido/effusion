defmodule Effusion.PWP.Handler do
  alias Effusion.PWP.Messages
  alias Effusion.PWP.Socket
  alias Effusion.BTP.DownloadServer
  use GenServer
  require Logger

  @behaviour :ranch_protocol

  @moduledoc """
  Ranch protocol handler for incoming PWP connections.
  """

  def start_link(ref, socket, transport, _opts) do
    pid = :proc_lib.spawn_link(__MODULE__, :init, [ref, socket, transport])
    {:ok, pid}
  end

  def init(init_arg) do
    {:ok, init_arg}
  end

  def init(ref, socket, transport) do
    _ = Logger.debug("Starting protocol")

    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, active: :once)
    :gen_server.enter_loop(__MODULE__, [], %{socket: socket, transport: transport})
  end

  def handle_info({:tcp, tcp_socket, data}, state = %{socket: socket, transport: transport}) when is_binary(data) do
    # _ = Logger.debug("Handling incoming packet with data #{inspect data}")
    {:ok, msg} = Messages.decode(data)

    handle_btp(msg, state)
  end

  def handle_info(
        {:btp_send, dest_peer_id, msg},
        state = %{socket: socket, remote_peer_id: peer_id}
      )
      when dest_peer_id == peer_id do
        Logger.debug "Sending message #{inspect msg} to peer #{peer_id}"
    case Socket.send_msg(socket, msg) do
      :ok -> {:noreply, state}
      {:error, reason} -> {:stop, {:send_failure, reason}, state}
    end
  end

  def handle_info({:tcp_closed, _socket}, state), do: {:stop, :normal, state}
  def handle_info(:disconnect, state), do: {:stop, :normal, state}


  def handle_info(info, state) do
    Logger.warn "Handler received unknown message: #{inspect info}"
    {:noreply, state}
  end

  def handle_btp({:handshake, remote_peer_id, info_hash, _reserved}, state = %{socket: socket, transport: transport}) do
    {:ok, address} = :inet.peername(socket)

    case Registry.lookup(SessionRegistry, info_hash) do
      [{_pid, _hash}] ->
        _ = Logger.debug("Successfully received connection from #{inspect address} for #{Effusion.Hash.inspect info_hash}")

        {:ok, _pid} = Registry.register(ConnectionRegistry, info_hash, remote_peer_id)
        :ok = DownloadServer.connected(info_hash, remote_peer_id, address)

        local_peer_id = Application.get_env(:effusion, :peer_id)
        _ = Logger.debug("Responding to #{remote_peer_id} with handshake")
        :ok = Socket.send_msg(socket, {:handshake, local_peer_id, info_hash})

        :ok = :inet.setopts(socket, active: :once, packet: 4)

        state = state
        |> Map.put(:socket, socket)
        |> Map.put(:info_hash, info_hash)
        |> Map.put(:remote_peer_id, remote_peer_id)

        {:noreply, state}
      [] ->
        {:stop, state}
    end
  end

  def handle_btp(msg, state = %{info_hash: info_hash, remote_peer_id: peer_id, socket: socket}) do
    Logger.debug "Handler received BTP message from peer: #{inspect msg}"

    :ok = DownloadServer.handle_message(info_hash, peer_id, msg)
    :ok = :inet.setopts(socket, active: :once)
    {:noreply, state}
  end


  def terminate(_, %{socket: socket, info_hash: info_hash, peer_id: peer_id, address: address}) do
    Socket.close(socket)
    DownloadServer.unregister_connection(info_hash, peer_id, address)
    :ok
  end
  def terminate(_, {nil, info_hash, peer_id, address}) do
    DownloadServer.unregister_connection(info_hash, peer_id, address)
  end

  def terminate(reason, _peer) do
    Logger.debug "Incoming connection handler terminating with reason #{inspect reason}"
    :ok
  end
end
