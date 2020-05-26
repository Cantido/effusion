defmodule Effusion.PWP.TCP.Connection do
  alias Effusion.BTP.Torrent
  alias Effusion.PWP.Messages
  alias Effusion.PWP.ProtocolHandler
  alias Effusion.PWP.TCP.Socket
  import Effusion.BTP.Peer, only: [is_peer_id: 1]
  import Effusion.Hash, only: [is_hash: 1]
  require Logger

  @moduledoc """
  Protocol handler for the Peer Wire Protocol.

  This amounts to performing the handshake, and then forwarding any messages
  to the associated download.
  """

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

  defp successful_handshake(socket, info_hash, peer_id, extensions) do
    with {:ok, address} <- :inet.peername(socket),
         :ok <- ProtocolHandler.handle_connect(info_hash, peer_id, address, extensions),
         :ok <- :inet.setopts(socket, active: :once, packet: 4) do
      :ok
    else
      err -> err
    end
  end

  defp connect(%{
      address: address = {host, port},
      info_hash: info_hash,
      expected_peer_id: expected_peer_id
    }) when is_integer(port) and is_hash(info_hash) and is_peer_id(expected_peer_id) do
    state = %{
      address: address,
      info_hash: info_hash,
      remote_peer_id: expected_peer_id
    }

    :telemetry.execute([:pwp, :outgoing, :starting], %{}, state)

    with {:ok, socket} <- :gen_tcp.connect(host, port, [:binary, active: false, keepalive: true], 10_000),

         :ok <- Socket.send_msg(socket, ProtocolHandler.get_handshake(info_hash)),
         {:ok, handshake = {:handshake, remote_peer_id, _remote_info_hash, extensions}} <- Socket.recv(socket, 68),
         :ok <- ProtocolHandler.recv_handshake(handshake, info_hash, expected_peer_id),
         :ok <- successful_handshake(socket, info_hash, remote_peer_id, extensions) do

      :telemetry.execute([:pwp, :outgoing, :success], %{}, state)

      {:ok, address} = :inet.peername(socket)
      {:noreply, %{
        socket: socket,
        info_hash: info_hash,
        remote_peer_id: remote_peer_id,
        address: address
      }}
    else
      {:error, reason} ->
        :telemetry.execute([:pwp, :outgoing, :failure], %{}, state)
        {:stop, reason, state}
    end
  end

  @doc """
  Handle a Peer Wire Protocol message.
  """
  def handle_btp(btp_message, state)

  def handle_btp(handshake = {:handshake, remote_peer_id, info_hash, extensions}, state = %{socket: socket})
    when is_peer_id(remote_peer_id)
     and is_hash(info_hash) do

    if Torrent.downloading?(info_hash) do
      :telemetry.execute(
        [:pwp, :incoming, :starting],
        %{},
        Map.put(state, :remote_peer_id, remote_peer_id)
      )

      with :ok <- ProtocolHandler.recv_handshake(handshake),
           :ok <- Socket.send_msg(socket, ProtocolHandler.get_handshake(info_hash)),
           :ok <- successful_handshake(socket, info_hash, remote_peer_id, extensions),
           {:ok, address} <- :inet.peername(socket) do

        :telemetry.execute([:pwp, :incoming, :success], %{}, state)

        {:noreply,
         %{
           socket: socket,
           info_hash: info_hash,
           remote_peer_id: remote_peer_id,
           address: address
         }}
      else
        _ ->
          :telemetry.execute([:pwp, :incoming, :failure], %{}, state)
          {:stop, :handshake_failure, state}
      end
    else
      {:stop, :normal, state}
    end
  end

  def handle_btp(msg, state = %{info_hash: info_hash, remote_peer_id: peer_id})
    when is_hash(info_hash)
     and is_peer_id(peer_id) do
    :ok = Effusion.BlockingQueue.push(MessageQueue, {info_hash, peer_id, msg})
    {:noreply, state}
  end

  def handle_info(
        {:btp_send, dest_peer_id, msg},
        state = %{socket: socket, remote_peer_id: peer_id}
      )
      when dest_peer_id == peer_id do
    :telemetry.execute([:pwp, :message_sent], %{}, %{message: msg, peer: peer_id})

    case Socket.send_msg(socket, msg) do
      :ok -> {:noreply, state}
      {:error, reason} -> {:stop, {:send_failure, reason}, state}
    end
  end

  def handle_info({:tcp, _tcp_socket, data}, state) when is_binary(data) do
    {latency, {msg, ret}} = :timer.tc(fn ->
      {:ok, msg} = Messages.decode(data)
      ret = handle_btp(msg, state)
      {msg, ret}
    end)

    :telemetry.execute(
      [:pwp, :message_received],
      %{latency: latency / 1_000},
      Map.take(state, [:remote_peer_id, :info_hash])
      |> Map.put(:binary, data)
      |> Map.put(:message, msg)
    )

    :ok = :inet.setopts(state.socket, active: :once)
    ret
  end

  def handle_info(:timeout, state), do: connect(state)
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

    if Map.has_key?(state, :info_hash) && Map.has_key?(state, :address) do
      ProtocolHandler.handle_disconnect(state.info_hash, state.address, reason)
    end

    :ok
  end
end
