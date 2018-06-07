defmodule Effusion.Application.PeerServer do
  use GenServer, restart: :temporary
  alias Effusion.Application.SessionServer
  alias Effusion.PWP.Socket
  alias Effusion.BTP.Peer
  require Logger
  @moduledoc """
  A connection to a peer.

  This connection sends messages back to the parent SessionServer containing
  completed blocks, expecting the parent SessionServer to keep track of them.
  It also makes the SessionServer responsible for selecting which pieces to request.
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
    {
      :ok,
      peer,
      0
    }
  end

  def handle_info(:timeout, state) do
    with {:ok, socket, state} <- Socket.connect(state),
         state <- Map.put(state, :socket, socket)
    do
      {:noreply, state}
    else
      _ -> {:stop, :failed_handshake, state}
    end
  end

  def handle_info({:tcp, _socket, data}, state) do
    handle_packet(data, state)
  end

  def handle_info({:tcp_closed, _socket}, state) do
    {:stop, :normal, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  defp handle_packet(data, state) do
    with {:ok, msg1} <- Socket.decode(data),
         :ok <- Logger.info "Got message #{inspect(msg1)}"
    do
      messages = SessionServer.handle_message(state.session, state.peer_id, msg1)
      Enum.map(messages, fn(m) -> :ok = Socket.send_msg(state.socket, m) end)
      {:noreply, state}
    else
      {:error, reason} -> {:stop, reason, state}
    end
  end

  def terminate(_, %{socket: socket}) do
    :gen_tcp.close(socket)
  end

  def terminate(_, _), do: :ok
end
