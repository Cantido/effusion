defmodule Effusion.PWP.Connection do
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

  def disconnect_all(info_hash) do
    Registry.dispatch(ConnectionRegistry, info_hash, fn connections ->
      connections
      |> Enum.map(fn {c, _p} -> disconnect(c) end)
    end)
  end

  def btp_broadcast(info_hash, message, peer_id_selector \\ fn _ -> true end) do
    :ok =
      Registry.dispatch(ConnectionRegistry, info_hash, fn connections ->
        connections
        |> Enum.filter(fn {_, peer_id} -> peer_id_selector.(peer_id) end)
        |> Enum.each(fn {c, id} -> send(c, {:btp_send, id, message}) end)
      end)
  end

  def btp_send(info_hash, :broadcast, message) do
    btp_broadcast(info_hash, message)
  end

  def btp_send(info_hash, peer_id, message) do
    connections = Registry.match(ConnectionRegistry, info_hash, peer_id)

    _ =
      case connections do
        [{conn_pid, ^peer_id}] -> send(conn_pid, {:btp_send, peer_id, message})
        [] -> []
      end
  end

  ## Callbacks

  def init(peer) do
    {:ok, peer, 0}
  end

  defp ntoa({host, port}) when is_binary(host) do
    "#{host}:#{port}"
  end

  defp ntoa({host, port}) do
    "#{:inet.ntoa(host)}:#{port}"
  end

  defp handshake(peer) do
    _ = Logger.debug("Establishing connection to #{ntoa(peer.address)}")

    case Socket.connect(peer) do
      {:ok, socket, peer} ->
        _ = Logger.debug("Successfully connected to #{ntoa(peer.address)}")
        {:ok, _pid} = Registry.register(ConnectionRegistry, peer.info_hash, peer.remote_peer_id)
        :ok = DownloadServer.connected(peer.info_hash, peer.remote_peer_id, peer.address)
        :ok = :inet.setopts(socket, active: :once)
        {:noreply, {socket, peer.info_hash, peer.remote_peer_id, peer.address}}

      {:error, reason} ->
        Logger.debug "Handshake with #{ntoa peer.address} failed: #{inspect reason}"
        {:stop, :normal,
         {nil, peer.info_hash, peer.remote_peer_id, peer.address}}
    end
  end

  def handle_packet(socket, data, {_socket, info_hash, peer_id, address}) do
    case Socket.decode(data) do
      {:ok, msg} ->
        :ok = DownloadServer.handle_message(info_hash, peer_id, msg)
        :ok = :inet.setopts(socket, active: :once)
        {:noreply, {socket, info_hash, peer_id, address}}

      {:error, reason} ->
        {:stop, {:bad_message, reason, data}, {socket, info_hash, peer_id, address}}
    end
  end

  def handle_info({:btp_send, msg}, state = {socket, _, _, _}) do
    case Socket.send_msg(socket, msg) do
      :ok -> {:noreply, state}
      {:error, reason} -> {:stop, {:send_failure, reason}, state}
    end
  end

  def handle_info(
        {:btp_send, dest_peer_id, msg},
        state = {_socket, _info_hash, peer_id, _address}
      )
      when dest_peer_id == peer_id do
    handle_info({:btp_send, msg}, state)
  end

  def handle_info(:timeout, peer), do: handshake(peer)
  def handle_info({:tcp, socket, data}, state), do: handle_packet(socket, data, state)
  def handle_info({:tcp_closed, _socket}, state), do: {:stop, :normal, state}
  def handle_info(:disconnect, state), do: {:stop, :normal, state}
  def handle_info(_, state), do: {:noreply, state}

  def terminate(_, {nil, info_hash, peer_id, address}) do
    DownloadServer.unregister_connection(info_hash, peer_id, address)
  end

  def terminate(_, {socket, info_hash, peer_id, address}) do
    Socket.close(socket)
    DownloadServer.unregister_connection(info_hash, peer_id, address)
    :ok
  end

  def terminate(_, _peer) do
    :ok
  end
end
