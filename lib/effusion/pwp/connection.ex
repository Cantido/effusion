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

  def handle_info(:timeout, peer) do
    case Socket.connect(peer) do
      {:ok, _socket, peer} -> {:ok, peer}
      _ -> {:stop, :failed_handshake, peer}
    end
  end

  def handle_info({:tcp, socket, data}, %{session: session, peer_id: peer_id} = state) do
    case Socket.decode(data) do
      {:ok, msg} ->
        messages = SessionServer.handle_message(session, peer_id, msg)
        Enum.map(messages, fn(m) -> :ok = Socket.send_msg(socket, m) end)
        {:noreply, state}
      {:error, reason} -> {:error, reason}
      err -> err
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
