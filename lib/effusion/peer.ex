require Logger

defmodule Effusion.Peer do
  use GenServer, restart: :temporary

  ## Client API

  def start_link(peer_id) do
    name = {:via, Registry, {Effusion.PeerRegistry, peer_id}}
    GenServer.start_link(__MODULE__, peer_id, name: name)
  end

  def start_link(_a, peer_id) do
    start_link(peer_id)
  end

  def id(peer) do
    GenServer.call(peer, {:get_id})
  end

  ## Server Callbacks

  def init(peer_id) do
    {:ok, %{id: peer_id}}
  end

  def handle_call({:get_id}, _from, state) do
    {:reply, state.id, state}
  end
end
