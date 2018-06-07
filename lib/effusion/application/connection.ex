defmodule Effusion.Application.Connection do
  use GenServer, restart: :temporary
  alias Effusion.Application.SessionServer
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

  def handle_info(:timeout, state) do
    case SessionServer.connect(state.session, state) do
      {:ok, state} -> {:noreply, state}
      _ -> {:stop, :failed_handshake, state}
    end
  end

  def handle_info({:tcp, socket, data}, %{session: session, peer_id: peer_id} = state) do
    case SessionServer.handle_packet(session, peer_id, data, socket) do
      :ok -> {:noreply, state}
      {:error, reason} -> {:stop, reason, state}
    end
  end

  def handle_info({:tcp_closed, _socket}, state) do
    {:stop, :normal, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  def terminate(_, %{socket: socket}) do
    :gen_tcp.close(socket)
  end

  def terminate(_, _), do: :ok
end
