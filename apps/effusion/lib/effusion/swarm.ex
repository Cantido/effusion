defmodule Effusion.Swarm do
  use GenServer
  alias Effusion.Peer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, [], opts)
  end

  def add_peer({host, port}) do
    add_peer(host, port)
  end

  def add_peer(swarm \\ __MODULE__, host, port) do
    GenServer.call(swarm, {:add_peer, host, port})
  end

  def peer_id(swarm \\ __MODULE__, address) do
    GenServer.call(swarm, {:peer_id, address})
  end

  def set_peer_id(swarm \\ __MODULE__, address, peer_id) do
    GenServer.call(swarm, {:set_peer_id, address, peer_id})
  end

  def increment_failcount(swarm \\ __MODULE__, address) do
    GenServer.call(swarm, {:increment_failcount, address})
  end

  def decrement_failcount(swarm \\ __MODULE__, address) do
    GenServer.call(swarm, {:decrement_failcount, address})
  end

  def add_torrent(swarm \\ __MODULE__, address, info_hash) do
    GenServer.call(swarm, {:add_torrent, address, info_hash})
  end

  def peers_for_torrent(swarm \\ __MODULE__, info_hash) do
    GenServer.call(swarm, {:peers_for_torrent, info_hash})
  end

  def init(_opts) do
    {:ok, %{peers: %{}, torrents: %{}}}
  end

  def handle_call({:add_peer, host, port}, _from, swarm) do
    peers = Map.put_new(swarm.peers, {host, port}, Peer.new({host, port}))
    {:reply, :ok, %{swarm | peers: peers}}
  end

  def handle_call({:peer_id, address}, _from, swarm) do
    if peer = Map.get(swarm.peers, address) do
      {:reply, Peer.peer_id(peer), swarm}
    else
      {:reply, nil, swarm}
    end
  end

  def handle_call({:set_peer_id, address, peer_id}, _from, swarm) do
    peers =
      Map.put_new(swarm.peers, address, Peer.new(address))
      |> Map.update!(address, &Peer.set_peer_id(&1, peer_id))

    {:reply, :ok, %{swarm | peers: peers}}
  end

  def handle_call({:increment_failcount, address}, _from, swarm) do
    peers =
      Map.put_new(swarm, address, Peer.new(address))
      |> Map.update!(address, &Peer.increment_failcount/1)

    {:reply, :ok, %{swarm | peers: peers}}
  end

  def handle_call({:decrement_failcount, address}, _from, swarm) do
    peers =
      Map.put_new(swarm, address, Peer.new(address))
      |> Map.update!(address, &Peer.decrement_failcount/1)

    {:reply, :ok, %{swarm | peers: peers}}
  end

  def handle_call({:add_torrent, address, info_hash}, _from, swarm) do
    torrents = Map.update(swarm.torrents, info_hash, MapSet.new([address]), &MapSet.put(&1, address))

    {:reply, :ok, %{swarm | torrents: torrents}}
  end

  def handle_call({:peers_for_torrent, info_hash}, _from, swarm) do
    addresses = Map.get(swarm.torrents, info_hash, MapSet.new()) |> Enum.to_list()
    peers = Map.take(swarm.peers, addresses) |> Map.values()

    {:reply, peers, swarm}
  end
end
