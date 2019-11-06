defmodule Effusion.PWP.Connection do
  alias Effusion.PWP.Messages
  alias Effusion.PWP.Socket
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.BTP.DownloadServer
  alias Effusion.Statistics.Net, as: NetStats
  alias Effusion.Statistics.Peer, as: PeerStats
  alias Effusion.Statistics.Session, as: SessionStats
  require Logger

  def disconnect(pid) do
    send(pid, :disconnect)
  end

  defp ntoa({host, port}) when is_binary(host) and port > 0 do
    "#{host}:#{port}"
  end

  defp ntoa({host, port}) when port > 0 do
    "#{:inet.ntoa(host)}:#{port}"
  end

  defp connect(peer) do
    _ = Logger.debug("Establishing connection to #{ntoa(peer.address)}")
    PeerStats.inc_num_tcp_peers() # must do it here in case we terminate later

    case Socket.connect(peer.address, peer.info_hash, peer.peer_id, peer.remote_peer_id) do
      {:ok, socket, remote_peer_id} ->
        _ = Logger.debug("Handshake with #{ntoa(peer.address)} successful")
        {:ok, _pid} = ConnectionRegistry.register(peer.info_hash, remote_peer_id)
        :ok = DownloadServer.connected(peer.info_hash, remote_peer_id, peer.address)
        :ok = :inet.setopts(socket, active: :once)
        {:noreply, %{
          socket: socket,
          info_hash: peer.info_hash,
          remote_peer_id: remote_peer_id,
          address: peer.address
        }}

      {:error, reason} ->
        Logger.debug "Handshake with #{ntoa peer.address} failed: #{inspect reason}"
        {:stop, :normal,
          %{
            info_hash: peer.info_hash,
            remote_peer_id: peer.remote_peer_id,
            address: peer.address
          }}
    end
  end

  def handle_btp({:handshake, remote_peer_id, info_hash, _reserved}, state = %{socket: socket}) do
    PeerStats.inc_num_tcp_peers()
    {:ok, address} = :inet.peername(socket)

    case Registry.lookup(SessionRegistry, info_hash) do
      [{_pid, _hash}] ->
        _ = Logger.debug("Successfully received connection from #{inspect address} for #{Effusion.Hash.inspect info_hash}")

        {:ok, _pid} = ConnectionRegistry.register(info_hash, remote_peer_id)
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
        {:stop, :info_hash_not_found, state}
    end
  end

  def handle_btp(msg, state = %{info_hash: info_hash, remote_peer_id: peer_id, socket: socket}) do
    Logger.debug "Handler received BTP message from peer: #{inspect msg}"

    SessionStats.inc_incoming_message(msg)
    :ok = DownloadServer.handle_message(info_hash, peer_id, msg)
    :ok = :inet.setopts(socket, active: :once)
    {:noreply, state}
  end

  def handle_info(
        {:btp_send, dest_peer_id, msg},
        state = %{socket: socket, remote_peer_id: peer_id}
      )
      when dest_peer_id == peer_id do
        Logger.debug "Sending message #{inspect msg} to peer #{peer_id}"
    SessionStats.inc_outgoing_message(msg)
    case Socket.send_msg(socket, msg) do
      :ok -> {:noreply, state}
      {:error, reason} -> {:stop, {:send_failure, reason}, state}
    end
  end

  def handle_info({:tcp, _tcp_socket, data}, state) when is_binary(data) do
    # _ = Logger.debug("Handling incoming packet with data #{inspect data}")
    Messages.payload_bytes_count(data) |> NetStats.add_recv_payload_bytes()
    byte_size(data) |> NetStats.add_recv_bytes()
    {:ok, msg} = Messages.decode(data)

    handle_btp(msg, state)
  end

  def handle_info(:timeout, peer) when is_map(peer), do: connect(peer)
  def handle_info({:tcp_closed, _socket}, state), do: {:stop, :normal, state}
  def handle_info(:disconnect, state), do: {:stop, :normal, state}

  def handle_info(info, state) do
    Logger.warn "Handler received unknown message: #{inspect info}"
    {:noreply, state}
  end

  def terminate(reason, %{socket: socket, info_hash: info_hash, remote_peer_id: remote_peer_id, address: address}) do
    PeerStats.dec_num_tcp_peers()
    Logger.debug "Connection handler for #{remote_peer_id} terminating with reason #{inspect reason}"

    Socket.close(socket)
    DownloadServer.unregister_connection(info_hash, address, reason)
    :ok
  end

  def terminate(reason, %{info_hash: info_hash, remote_peer_id: remote_peer_id, address: address}) do
    PeerStats.dec_num_tcp_peers()
    Logger.debug "Connection handler for #{remote_peer_id} terminating with reason #{inspect reason}"

    DownloadServer.unregister_connection(info_hash, address, reason)
  end

  def terminate(reason, %{remote_peer_id: remote_peer_id}) do
    PeerStats.dec_num_tcp_peers()
    Logger.debug "Connection handler for #{remote_peer_id} terminating with reason #{inspect reason}"

    :ok
  end

  def terminate(reason, _state) do
    PeerStats.dec_num_tcp_peers()
    Logger.debug "Connection handler terminating with reason #{inspect reason}"

    :ok
  end
end
