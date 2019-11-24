defmodule Effusion.BTP.Swarm do
  alias Effusion.BTP.Block
  alias Effusion.BTP.Peer
  import Effusion.BTP.Peer, only: [is_peer_id: 1]
  require Logger

  @moduledoc """
  A collection of peers.

  Functions that manipulate or query the peers as a whole are located here.
  """

  defstruct [
    :peer_id,
    :info_hash,
    :peers,
    :peer_addresses,
    :requested_block_peers
  ]

  def new(peer_id, info_hash) do
    %__MODULE__{
      peer_id: peer_id,
      info_hash: info_hash,
      # {peer_address, peer}
      peers: Map.new(),
      # {peer_address, peer_id}
      peer_addresses: Map.new(),
      # {block_id => [peer_id]}
      requested_block_peers: Map.new()
    }
  end

  def select_peer(swarm, peer_id) when is_peer_id(peer_id) do
    peer =
      Enum.find(swarm.peers, fn {_addr, peer} ->
        peer.peer_id == peer_id
      end)

    case peer do
      nil -> nil
      {_addr, peer} -> peer
    end
  end

  def put_peer(swarm, peer) do
    peers = Map.put(swarm.peers, peer.address, peer)
    addresses = Map.put(swarm.peer_addresses, peer.address, peer.peer_id)

    %{swarm | peers: peers, peer_addresses: addresses}
  end

  def peers(swarm = %__MODULE__{}) do
    Map.values(swarm.peers)
  end

  def peer_for_address(swarm = %__MODULE__{}, address) do
    Map.get(swarm.peers, address)
  end

  def availability_map(peers) do
    peers
    |> Enum.map(&Peer.availability_map/1)
    |> Enum.reduce(Map.new(), fn peer_availability_map, accumulator ->
      Map.merge(accumulator, peer_availability_map)
    end)
  end

  def add(swarm = %__MODULE__{}, peers) do
    known_addrs = Map.keys(swarm.peers)
    {known_ids, _} = Map.split(swarm.peer_addresses, known_addrs)

    new_peers =
      peers
      |> Enum.filter(&valid_peer?/1)
      |> Enum.map(fn p ->
        Peer.new({p.ip, p.port}, Map.get(p, :peer_id, nil))
      end)
      |> Enum.reject(&Enum.member?(known_ids, &1.peer_id))
      |> Enum.reject(&Enum.member?(known_addrs, &1.address))
      |> Map.new(&{&1.address, &1})

    announced_addrs =
      peers
      |> Enum.map(&{&1.ip, &1.port})
      |> Enum.uniq()
      |> Enum.into([])

    all_peers = Map.merge(new_peers, swarm.peers)
    {dec_failcount, keep_failcount} = Map.split(all_peers, announced_addrs)

    all_updated_peers =
      dec_failcount
      |> Enum.map(fn {addr, p} -> {addr, Peer.dec_fail_count(p)} end)
      |> Map.new()
      |> Map.merge(keep_failcount)

    peer_addresses =
      all_updated_peers
      |> Enum.reject(fn {_addr, p} -> p.peer_id == nil end)
      |> Map.new(fn {addr, p} -> {p.peer_id, addr} end)

    swarm
    |> Map.put(:peers, all_updated_peers)
    |> Map.put(:peer_addresses, peer_addresses)
  end

  def valid_peer?(%{port: port}) when port in 1..65_535, do: true
  def valid_peer?(_), do: false

  def requested_blocks(swarm) do
    swarm.peers
    |> Enum.flat_map(fn {_addr, p} ->
      Enum.map(p.blocks_we_requested, fn b ->
        {p.peer_id, b}
      end)
    end)
  end

  # requests already made: {block => [peer_id]}
  # Consider this as an inversion of the peer => [block_id] map
  def get_request_peers(swarm) do
    swarm.requested_block_peers
  end

  def remove_requested_block(swarm, block_id) do
    peers =
      swarm.peers
      |> Enum.map(fn {addr, p} ->
        p = Peer.remove_requested_block(p, block_id)
        {addr, p}
      end)
      |> Map.new()

    requested_block_peers = Map.delete(swarm.requested_block_peers, block_id)

    %{swarm | peers: peers, requested_block_peers: requested_block_peers}
  end

  def mark_blocks_requested(swarm, blocks) do
    Enum.reduce(blocks, swarm, fn {peer_id, block_id}, swarm_acc ->
      mark_block_requested(swarm_acc, peer_id, block_id)
    end)
  end

  def mark_block_requested(swarm = %__MODULE__{}, peer_id, block_id) do
    addr = Map.get(swarm.peer_addresses, peer_id)
    peers = Map.update!(swarm.peers, addr, &Peer.request_block(&1, block_id))
    swarm = %{swarm | peers: peers}

    requested_block_peers = Map.update(swarm.requested_block_peers, block_id, MapSet.new([peer_id]), &MapSet.put(&1, block_id))

    %{swarm | requested_block_peers: requested_block_peers}
  end

  def cancel_block_requests(swarm, block, from) when is_peer_id(from) do
    block_id = Block.id(block)

    cancel_messages =
      requested_blocks(swarm)
      |> Enum.reject(fn {peer_id, %{index: i, offset: o, size: s}} ->
        peer_id == from || (block_id.index == i && block_id.offset == o && block_id.size == s)
      end)
      |> Enum.map(fn {peer_id, _blk} -> peer_id end)
      |> Enum.map(&{:btp_send, &1, {:cancel, block_id}})
      |> Enum.uniq()

    Logger.debug("Swarm preparing cancel messages: #{inspect(cancel_messages)}")
    swarm = remove_requested_block(swarm, block_id)

    {swarm, cancel_messages}
  end

  def drop_requests(swarm, from) when is_peer_id(from) do
    peer = swarm
    |> select_peer(from)
    |> Peer.drop_requests()

    put_peer(swarm, peer)
  end

  def delegate_message(swarm = %__MODULE__{}, remote_peer_id, msg) do
    case get_connected_peer(swarm, remote_peer_id) do
      {:ok, peer} ->
        {peer, responses} = Peer.recv(peer, msg)
        swarm = Map.update(swarm, :peers, Map.new(), &Map.put(&1, peer.address, peer))
        {swarm, Enum.map(responses, fn r -> {:btp_send, remote_peer_id, r} end)}

      _ ->
        {swarm, []}
    end
  end

  def get_connected_peer(swarm = %__MODULE__{}, remote_peer_id)
      when is_peer_id(remote_peer_id) do
    case Map.fetch(swarm.peer_addresses, remote_peer_id) do
      {:ok, address} -> Map.fetch(swarm.peers, address)
      _ -> {:error, :peer_not_found}
    end
  end

  def handle_connect(swarm = %__MODULE__{}, peer_id, address) do
    swarm
    |> Map.update!(:peers, &put_connected_peer(&1, peer_id, address))
    |> Map.update!(:peer_addresses, &Map.put(&1, peer_id, address))
  end

  defp put_connected_peer(peers, peer_id, address) do
    peers
    |> Map.put_new(address, Peer.new(address, peer_id))
    |> Map.update!(address, fn peer ->
      peer
      |> Peer.dec_fail_count()
      |> Map.put(:peer_id, peer_id)
    end)
  end

  def handle_disconnect(swarm = %__MODULE__{}, address, reason \\ :normal) do
    swarm
    |> Map.update!(
      :peers,
      &Map.update(&1, address, Peer.new(address), fn peer ->
        Peer.inc_fail_count(peer)
      end)
    )
  end
end
