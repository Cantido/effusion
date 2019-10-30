defmodule Effusion.BTP.Swarm do
  alias Effusion.BTP.Peer
  import Effusion.BTP.Peer
  require Logger

  def new(peer_id, info_hash) do
    %{
      peer_id: peer_id,
      info_hash: info_hash,
      peers: Map.new(),
      peer_addresses: Map.new(),
      connected_peers: Map.new(),
      closed_connections: MapSet.new(),
    }
  end

  def add(swarm, peers) do
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

  def delegate_message(swarm, remote_peer_id, msg) do
    with {:ok, peer} <- get_connected_peer(swarm, remote_peer_id),
         {peer, responses} <- Peer.recv(peer, msg),
         swarm = Map.update(swarm, :peers, Map.new(), &Map.put(&1, peer.address, peer)) do
      {swarm, Enum.map(responses, fn r -> {:btp_send, remote_peer_id, r} end)}
    else
      _ -> {swarm, []}
    end
  end

  def get_connected_peer(swarm, remote_peer_id)
       when is_peer_id(remote_peer_id) do
    with {:ok, address} <- Map.fetch(swarm.peer_addresses, remote_peer_id) do
      Map.fetch(swarm.peers, address)
    else
      _ -> {:error, :peer_not_found}
    end
  end

  def handle_connect(swarm, peer_id, address) do
    swarm
    |> Map.update(:peers, Map.new(), &Map.put(&1, address, peer(swarm, peer_id, address)))
    |> Map.update(:peer_addresses, Map.new(), &Map.put(&1, peer_id, address))
  end

  def handle_disconnect(swarm, address) do
    swarm
    |> Map.update!(:peers, &Map.delete(&1, address))
  end

  defp peer(swarm, peer_id, peer_address)
       when is_peer_id(peer_id) do
    Peer.new(peer_address, swarm.peer_id, swarm.info_hash)
    |> Map.put(:remote_peer_id, peer_id)
  end
end
