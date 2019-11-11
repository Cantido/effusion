defmodule Effusion.PWP.Connection do
  alias Effusion.BTP.DownloadServer
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.PWP.Messages
  alias Effusion.PWP.Socket
  alias Effusion.Statistics.Net, as: NetStats
  alias Effusion.Statistics.Peer, as: PeerStats
  alias Effusion.Statistics.Session, as: SessionStats
  require Logger

  @moduledoc """
  Protocol handler for the Peer Wire Protocol.

  This amounts to performing the handshake, and then forwarding any messages
  to the associated download.
  """

  def disconnect(pid) do
    send(pid, :disconnect)
  end

  defp ntoa({host, port}) when is_binary(host) and port > 0 do
    "#{host}:#{port}"
  end

  defp ntoa({host, port}) when port > 0 do
    "#{:inet.ntoa(host)}:#{port}"
  end

  defp validate_info_hash(local_info_hash, remote_info_hash) do
    if local_info_hash == remote_info_hash do
      :ok
    else
      {:error, {:mismatched_info_hash, [expected: local_info_hash, actual: remote_info_hash]}}
    end
  end

  defp validate_peer_id(expected_peer_id, remote_peer_id) do
    if expected_peer_id == nil or expected_peer_id == remote_peer_id do
      :ok
    else
      {:error, {:mismatched_peer_id, [expected: expected_peer_id, actual: remote_peer_id]}}
    end
  end

  defp successful_handshake(socket, info_hash, peer_id) do
    with {:ok, address} <- :inet.peername(socket),
         _ = Logger.debug("Handshake with #{ntoa(address)} successful"),
         {:ok, _pid} <- ConnectionRegistry.register(info_hash, peer_id),
         :ok <- DownloadServer.connected(info_hash, peer_id, address),
         :ok <- :inet.setopts(socket, active: :once, packet: 4) do
      :ok
    else
      err -> err
    end
  end

  defp connect({address = {host, port}, info_hash, local_peer_id, expected_peer_id}) do
    _ = Logger.debug("Establishing connection to #{ntoa(address)}")
    # must do it here in case we terminate later
    PeerStats.inc_num_tcp_peers()

    state = %{
      address: address,
      info_hash: info_hash,
      remote_peer_id: expected_peer_id
    }

    with {:ok, socket} <- :gen_tcp.connect(host, port, [:binary, active: false, keepalive: true], 10_000),
         :ok <- Socket.send_msg(socket, {:handshake, local_peer_id, info_hash}),
         {:ok, {:handshake, remote_peer_id, remote_info_hash, _}} <- Socket.recv(socket, 68),
         :ok <- validate_info_hash(info_hash, remote_info_hash),
         :ok <- validate_peer_id(expected_peer_id, remote_peer_id),
         :ok <- successful_handshake(socket, info_hash, remote_peer_id) do
      {:noreply, Map.put(state, :socket, socket)}
    else
      {:error, :closed} ->
        {:stop, :normal, state}

      {:error, reason} ->
        {:stop, reason, state}
    end
  end

  def handle_btp({:handshake, remote_peer_id, info_hash, _reserved}, state = %{socket: socket}) do
    PeerStats.inc_num_tcp_peers()

    with [{_pid, _hash}] <- Registry.lookup(SessionRegistry, info_hash),
         local_peer_id = Application.get_env(:effusion, :peer_id),
         _ = Logger.debug("Responding to #{remote_peer_id} with handshake"),
         :ok <- Socket.send_msg(socket, {:handshake, local_peer_id, info_hash}),
         :ok <- successful_handshake(socket, info_hash, remote_peer_id) do
      {:noreply,
       %{
         socket: socket,
         info_hash: info_hash,
         remote_peer_id: remote_peer_id,
         address: :inet.peername(socket)
       }}
    else
      [] ->
        {:stop, :normal, state}

      {:error, reason} ->
        {:stop, reason, state}
    end
  end

  def handle_btp(msg, state = %{info_hash: info_hash, remote_peer_id: peer_id, socket: socket}) do
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
    Logger.debug("Sending message #{inspect(msg)} to peer #{peer_id}")
    SessionStats.inc_outgoing_message(msg)

    case Socket.send_msg(socket, msg) do
      :ok -> {:noreply, state}
      {:error, reason} -> {:stop, {:send_failure, reason}, state}
    end
  end

  def handle_info({:tcp, _tcp_socket, data}, state) when is_binary(data) do
    # _ = Logger.debug("Handling incoming packet with data #{inspect data}")
    Messages.payload_bytes_count(data) |> NetStats.add_recv_payload_bytes()
    NetStats.add_recv_bytes(byte_size(data))
    if Map.has_key?(state, :remote_peer_id) do
      NetStats.add_recv_bytes(state.remote_peer_id, byte_size(data))
    end
    {:ok, msg} = Messages.decode(data)

    handle_btp(msg, state)
  end

  def handle_info(:timeout, state), do: connect(state)
  def handle_info({:tcp_closed, _socket}, state), do: {:stop, :normal, state}
  def handle_info(:disconnect, state), do: {:stop, :normal, state}

  def handle_info(info, state) do
    Logger.warn("Handler received unknown message: #{inspect(info)}")
    {:noreply, state}
  end

  def terminate(reason, %{
        socket: socket,
        info_hash: info_hash,
        remote_peer_id: remote_peer_id,
        address: address
      }) do
    PeerStats.dec_num_tcp_peers()

    Logger.debug(
      "Connection handler for #{remote_peer_id} terminating with reason #{inspect(reason)}"
    )

    Socket.close(socket)
    DownloadServer.unregister_connection(info_hash, address, reason)
    :ok
  end

  def terminate(reason, %{info_hash: info_hash, remote_peer_id: remote_peer_id, address: address}) when not is_nil(remote_peer_id) and not is_nil(address) do
    PeerStats.dec_num_tcp_peers()

    Logger.debug(
      "Connection handler for #{inspect address} (#{inspect remote_peer_id}) terminating with reason #{inspect(reason)}"
    )

    DownloadServer.unregister_connection(info_hash, address, reason)
  end

  def terminate(reason, %{address: address}) when not is_nil(address) do
    PeerStats.dec_num_tcp_peers()

    Logger.debug(
      "Connection handler for #{inspect address} terminating with reason #{inspect(reason)}"
    )

    :ok
  end

  def terminate(reason, _state) do
    PeerStats.dec_num_tcp_peers()
    Logger.debug("Connection handler terminating with reason #{inspect(reason)}")

    :ok
  end
end
