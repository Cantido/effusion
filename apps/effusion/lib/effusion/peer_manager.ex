defmodule Effusion.PeerManager do
  use GenServer
  alias Effusion.Peer
  require Logger

  def add_peer(host, port) do
    DynamicSupervisor.start_child(Effusion.PeerSupervisor, {__MODULE__, %Peer{host: host, port: port}})
  end

  def start_link(peer) do
    GenServer.start_link(
      __MODULE__,
      peer,
      name: via({peer.host, peer.port})
    )
  end

  def peer_id(address) do
    GenServer.call(via(address), :peer_id)
  end

  def set_peer_id(address, peer_id) do
    GenServer.call(via(address), {:set_peer_id, peer_id})
  end

  def increment_failcount(address) do
    GenServer.call(via(address), :increment_failcount)
  end

  def decrement_failcount(address) do
    GenServer.call(via(address), :decrement_failcount)
  end

  defp via(address) do
    {:via, Registry, {PeerRegistry, address}}
  end

  def init(peer) do
    {:ok, peer}
  end

  def handle_call(:peer_id, _from, peer) do
    {:reply, Peer.peer_id(peer), peer}
  end

  def handle_call({:set_peer_id, peer_id}, _from, peer) do
    {:reply, :ok, Peer.set_peer_id(peer, peer_id)}
  end

  def handle_call(:increment_failcount, _from, peer) do
    {:reply, :ok, Peer.increment_failcount(peer)}
  end

  def handle_call(:decrement_failcount, _from, peer) do
    {:reply, :ok, Peer.decrement_failcount(peer)}
  end
end
