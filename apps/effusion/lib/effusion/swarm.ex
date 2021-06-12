defmodule Effusion.Swarm do
  use GenServer
  alias Effusion.Peer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, [], opts)
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

  def init(_opts) do
    {:ok, %{}}
  end

  def handle_call({:add_peer, host, port}, _from, swarm) do
    swarm = Map.put_new(swarm, {host, port}, Peer.new({host, port}))
    {:reply, :ok, swarm}
  end

  def handle_call({:peer_id, address}, _from, swarm) do
    if peer = Map.get(swarm, address) do
      {:reply, Peer.peer_id(peer), swarm}
    else
      {:reply, nil, swarm}
    end
  end

  def handle_call({:set_peer_id, address, peer_id}, _from, swarm) do
    swarm =
      Map.put_new(swarm, address, Peer.new(address))
      |> Map.update!(address, &Peer.set_peer_id(&1, peer_id))

    {:reply, :ok, swarm}
  end

  def handle_call({:increment_failcount, address}, _from, swarm) do
    swarm =
      Map.put_new(swarm, address, Peer.new(address))
      |> Map.update!(address, &Peer.increment_failcount/1)

    {:reply, :ok, swarm}
  end

  def handle_call({:decrement_failcount, address}, _from, swarm) do
    swarm =
      Map.put_new(swarm, address, Peer.new(address))
      |> Map.update!(address, &Peer.decrement_failcount/1)

    {:reply, :ok, swarm}
  end
end
