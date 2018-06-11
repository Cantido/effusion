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

  defp ntoa({host, port}) do
    "#{:inet.ntoa(host)}:#{port}"
  end

  def handle_info(:timeout, peer) do
    Logger.debug("Establishing connection to #{ntoa(peer.address)}")
    case Socket.connect(peer) do
      {:ok, socket, peer} ->
        Logger.debug("Successfully connected to #{ntoa(peer.address)}")
        :ok = :inet.setopts(socket, active: true)
        {:noreply, peer}
      {:error, reason} ->
        {:stop, {:failed_handshake, reason}, peer}
    end
  end

  def handle_info({:tcp, socket, data}, %{session: session, peer_id: peer_id} = state) do
    case Socket.decode(data) do
      {:ok, msg} ->
        Logger.debug("Got a message!!! #{inspect(msg)}")
        messages = SessionServer.handle_message(session, peer_id, msg)
        Logger.debug("replying: #{inspect(messages)}")
        :ok = Socket.send_all(socket, messages)
        {:noreply, state}
      {:error, reason} -> {:stop, {:bad_message, reason, data}, state}
    end
  end

  def handle_info({:tcp_closed, _socket}, state) do
    {:stop, :normal, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  def terminate(_, %{socket: socket}) do
    Socket.close(socket)
  end

  def terminate(_, _), do: :ok
end
