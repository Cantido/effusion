defmodule Effusion.PWP.Connection do
  use GenServer, restart: :temporary
  alias Effusion.BTP.SessionServer
  alias Effusion.PWP.Socket
  require Logger
  @moduledoc """
  A connection to a peer.

  Allows for network IO to happen in a separate process,
  and sends PWP messages back to the parent session server.
  """

  ## API

  def connect(peer) do
    Effusion.Application.ConnectionSupervisor.start_child(peer)
  end

  def start_link(peer) do
    GenServer.start_link(__MODULE__, peer)
  end

  ## Callbacks

  def init(peer) do
    { :ok, peer, 0 }
  end

  defp ntoa({host, port}) when is_binary(host) do
    "#{host}:#{port}"
  end

  defp ntoa({host, port}) do
    "#{:inet.ntoa(host)}:#{port}"
  end

  def handshake(peer) do
    Logger.debug("Establishing connection to #{ntoa(peer.address)}")
    case Socket.connect(peer) do
      {:ok, socket, peer} ->
        Logger.debug("Successfully connected to #{ntoa(peer.address)}")
        :ok = SessionServer.register_connection(peer.session, peer.remote_peer_id, peer.address)
        :ok = :inet.setopts(socket, active: :once)
        {:noreply, {socket, peer.session, peer.remote_peer_id}}
      {:error, reason} ->
        {:stop, {:failed_handshake, reason}, peer}
    end
  end

  def handle_packet(socket, data, {_socket, session, peer_id}) do
    case Socket.decode(data) do
      {:ok, msg} ->
        Logger.debug("Got a message!!! #{inspect(msg)}")
        messages = SessionServer.handle_message(session, peer_id, msg)
        Logger.debug("replying: #{inspect(messages)}")
        :ok = Socket.send_all(socket, messages)
        :ok = :inet.setopts(socket, active: :once)
        {:noreply, {socket, session, peer_id}}
      {:error, reason} ->
        {:stop, {:bad_message, reason, data}, {socket, session, peer_id}}
    end
  end

  def handle_info(:timeout, peer), do: handshake(peer)
  def handle_info({:tcp, socket, data}, state), do: handle_packet(socket, data, state)
  def handle_info({:tcp_closed, _socket}, state), do: {:stop, :normal, state}
  def handle_info(_, state), do: {:noreply, state}

  def terminate(_, {socket, session, peer_id}) do
    Socket.close(socket)
    SessionServer.unregister_connection(session, peer_id)
    :ok
  end
end
