defmodule Effusion.Application.PeerServer do
  use GenServer, restart: :temporary
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

  def connect(address, peer_id, info_hash, session) when is_binary(peer_id) and byte_size(peer_id) == 20 and is_binary(info_hash) and byte_size(info_hash) == 20 and is_pid(session) do
    args = [address, peer_id, info_hash, session]
    Effusion.Application.ConnectionSupervisor.start_child(args)
  end

  def start_link([address, peer_id, info_hash, session]) when is_pid(session) do
    GenServer.start_link(__MODULE__, [address, peer_id, info_hash, session])
  end

  ## Callbacks

  def init([address, peer_id, info_hash, session]) when is_pid(session) do
    {
      :ok,
      Peer.new(address, peer_id, info_hash, session),
      0
    }
  end

  def handle_info(:timeout, state) do
    _ = Logger.info("in timeout block")
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
      {state, messages} = Peer.recv(state, msg1)
      Enum.map(messages, fn(m) -> :ok = Socket.send(state.socket, m) end)
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
