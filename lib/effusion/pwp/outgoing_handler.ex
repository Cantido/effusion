defmodule Effusion.PWP.OutgoingHandler do
  use GenServer, restart: :temporary
  alias Effusion.BTP.DownloadServer
  alias Effusion.PWP.Socket
  require Logger

  @moduledoc """
  A connection to a peer.

  Allows for network IO to happen in a separate process,
  and sends PWP messages back to the parent session server.
  """

  ## API

  @doc """
  Start a connection to a `peer` in the Connection supervision hierarchy.
  """
  def connect(peer) do
    Effusion.Application.ConnectionSupervisor.start_child(peer)
  end

  @doc """
  Start a connection to a `peer`, and link the resulting process to the current process.
  """
  def start_link(peer) do
    GenServer.start_link(__MODULE__, peer)
  end

  def disconnect(pid) do
    send(pid, :disconnect)
  end

  ## Callbacks

  def init(peer) do
    {:ok, peer, 0}
  end

  defp ntoa({host, port}) when is_binary(host) and port > 0 do
    "#{host}:#{port}"
  end

  defp ntoa({host, port}) when port > 0 do
    "#{:inet.ntoa(host)}:#{port}"
  end

  defp handshake(peer) do
    _ = Logger.debug("Establishing connection to #{ntoa(peer.address)}")

    case Socket.connect(peer.address, peer.info_hash, peer.peer_id, peer.remote_peer_id) do
      {:ok, socket, remote_peer_id} ->
        _ = Logger.debug("Handshake with #{ntoa(peer.address)} successful")
        {:ok, _pid} = Registry.register(ConnectionRegistry, peer.info_hash, remote_peer_id)
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

  def handle_packet(socket, data, state = %{info_hash: info_hash, remote_peer_id: peer_id}) do
    case Socket.decode(data) do
      {:ok, msg} ->
        :ok = DownloadServer.handle_message(info_hash, peer_id, msg)
        :ok = :inet.setopts(socket, active: :once)
        {:noreply, state}

      {:error, reason} ->
        {:stop, {:bad_message, reason, data}, state}
    end
  end

  def handle_info({:btp_send, msg}, state = %{socket: socket}) do
    case Socket.send_msg(socket, msg) do
      :ok -> {:noreply, state}
      {:error, reason} -> {:stop, {:send_failure, reason}, state}
    end
  end

  def handle_info(
        {:btp_send, dest_peer_id, msg},
        state = %{remote_peer_id: peer_id}
      )
      when dest_peer_id == peer_id do
    handle_info({:btp_send, msg}, state)
  end

  def handle_info(:timeout, peer) when is_map(peer), do: handshake(peer)
  def handle_info({:tcp, socket, data}, state), do: handle_packet(socket, data, state)
  def handle_info({:tcp_closed, _socket}, state), do: {:stop, :normal, state}
  def handle_info(:disconnect, state), do: {:stop, :normal, state}
  def handle_info(_, state), do: {:noreply, state}

  def terminate(reason, %{socket: socket, info_hash: info_hash, remote_peer_id: remote_peer_id, address: address}) do
    Logger.debug "Connection handler for #{remote_peer_id} terminating with reason #{inspect reason}"

    Socket.close(socket)
    DownloadServer.unregister_connection(info_hash, remote_peer_id, address)
    :ok
  end

  def terminate(reason, %{info_hash: info_hash, remote_peer_id: remote_peer_id, address: address}) do
    Logger.debug "Connection handler for #{remote_peer_id} terminating with reason #{inspect reason}"

    DownloadServer.unregister_connection(info_hash, remote_peer_id, address)
  end

  def terminate(reason, %{info_hash: info_hash, remote_peer_id: remote_peer_id}) do
    Logger.debug "Connection handler for #{remote_peer_id} terminating with reason #{inspect reason}"

    :ok
  end
end
