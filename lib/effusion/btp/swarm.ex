defmodule Effusion.BTP.Swarm do
  alias Effusion.BTP.Peer
  import Effusion.BTP.Peer
  require Logger

  defstruct [
    :peer_id,
    :info_hash,
    :peers,
    :peer_addresses,
    :requested_pieces
  ]

  def new(peer_id, info_hash) do
    %__MODULE__{
      peer_id: peer_id,
      info_hash: info_hash,
      peers: Map.new(), # {peer_address, peer}
      peer_addresses: Map.new(), # {peer_address, peer_id}
      requested_pieces: MapSet.new()
    }
  end

  def peers(swarm = %__MODULE__{}) do
    Map.values(swarm.peers)
  end

  def peer_for_address(swarm = %__MODULE__{}, address) do
    Map.get(swarm.peers, address)
  end

  def add(swarm = %__MODULE__{}, peers) do
    known_addrs = Map.keys(swarm.peers)
    {known_ids, _} = Map.split(swarm.peer_addresses, known_addrs)

    new_peers =
      peers
      |> Enum.filter(&valid_peer?/1)
      |> Enum.map(fn p ->
        Peer.new({p.ip, p.port}, swarm.peer_id, swarm.info_hash)
        |> Peer.set_remote_peer_id(Map.get(p, :peer_id, nil))
      end)
      |> Enum.reject(&Enum.member?(known_ids, &1.remote_peer_id))
      |> Enum.reject(&Enum.member?(known_addrs, &1.address))
      |> Map.new(&({&1.address, &1}))

    announced_addrs = peers
    |> Enum.map(&({&1.ip, &1.port}))
    |> Enum.uniq()
    |> Enum.into([])

    all_peers = Map.merge(new_peers, swarm.peers)
    {dec_failcount, keep_failcount} = Map.split(all_peers, announced_addrs)

    all_updated_peers = dec_failcount
    |> Enum.map(fn {addr, p} -> {addr, Peer.dec_fail_count(p)} end)
    |> Map.new()
    |> Map.merge(keep_failcount)

    peer_addresses = all_updated_peers
    |> Enum.reject(fn {_addr, p} -> p.remote_peer_id == nil end)
    |> Map.new(fn {addr, p} -> {p.remote_peer_id, addr} end)

    swarm
    |> Map.put(:peers, all_updated_peers)
    |> Map.put(:peer_addresses, peer_addresses)
  end

  def valid_peer?(%{port: port}) when port in 1..65535, do: true
  def valid_peer?(_), do: false

  def requested_blocks(swarm) do
    swarm.peers
    |> Enum.flat_map(fn {_addr, p} ->
      Enum.map(p.blocks_we_requested, fn b ->
        {p.remote_peer_id, b}
      end)
    end)
  end

  def remove_requested_block(swarm, block_id) do
    Map.update!(swarm, :requested_pieces, &remove_requested_block_from_set(&1, block_id))
  end

  defp remove_requested_block_from_set(set, %{index: id_i, offset: id_o, size: id_s}) do
    set
    |> Enum.filter(fn {_peer, %{index: i, offset: o, size: s}} ->
      i == id_i && o == id_o && s == id_s
    end)
    |> MapSet.new()
  end

  def mark_block_requested(swarm = %__MODULE__{}, peer_id, block_id) do
    addr = Map.get(swarm.peer_addresses, peer_id)
    peers = Map.update!(swarm.peers, addr, &Peer.request_block(&1, block_id))
    %{swarm | peers: peers}
  end

  def delegate_message(swarm = %__MODULE__{}, remote_peer_id, msg) do
    with {:ok, peer} <- get_connected_peer(swarm, remote_peer_id),
         {peer, responses} <- Peer.recv(peer, msg),
         swarm = Map.update(swarm, :peers, Map.new(), &Map.put(&1, peer.address, peer)) do
      {swarm, Enum.map(responses, fn r -> {:btp_send, remote_peer_id, r} end)}
    else
      _ -> {swarm, []}
    end
  end

  def get_connected_peer(swarm = %__MODULE__{}, remote_peer_id)
       when is_peer_id(remote_peer_id) do
    with {:ok, address} <- Map.fetch(swarm.peer_addresses, remote_peer_id) do
      Map.fetch(swarm.peers, address)
    else
      _ -> {:error, :peer_not_found}
    end
  end

  def handle_connect(swarm = %__MODULE__{}, peer_id, address) do
    swarm
    |> Map.update!(:peers, &update_connected_peer(&1, swarm, peer_id, address))
    |> Map.update!(:peer_addresses, &Map.put(&1, peer_id, address))
  end

  defp update_connected_peer(peers, swarm, peer_id, address) do
    peers
    |> Map.put_new(address,  peer(swarm, peer_id, address))
    |> Map.update!(address, fn peer ->
      peer
      |> Peer.dec_fail_count()
      |> Peer.set_remote_peer_id(peer_id)
    end)
  end

  def handle_disconnect(swarm = %__MODULE__{}, address, reason \\ :normal) do
    swarm
    |> Map.update!(:peers, &Map.update!(&1, address, fn peer ->
      if reason != :normal do
        Peer.inc_fail_count(peer)
      else
        peer
      end
    end))
  end

  defp peer(swarm = %__MODULE__{}, peer_id, peer_address)
       when is_peer_id(peer_id) do
    Peer.new(peer_address, swarm.peer_id, swarm.info_hash)
    |> Map.put(:remote_peer_id, peer_id)
  end
end
