defmodule Effusion.BTP.Swarm do
  alias Effusion.BTP.Peer
  import Effusion.BTP.Peer
  require Logger

  def new(local_peer_id, info_hash) do
    %{
      peer_id: local_peer_id,
      info_hash: info_hash,
      peers: Map.new(),
      peer_addresses: Map.new(),
      connected_peers: Map.new(),
      closed_connections: MapSet.new()
    }
  end

  def add_peers(d, peers) do
    known_addrs = Map.keys(d.peers)
    {known_ids, _} = Map.split(d.peer_addresses, known_addrs)

    new_peers =
      peers
      |> Enum.map(fn p ->
        Peer.new({p.ip, p.port}, d.peer_id, d.info_hash, self())
        |> Peer.set_remote_peer_id(Map.get(p, :peer_id, nil))
      end)
      |> Enum.reject(&Enum.member?(known_ids, &1.remote_peer_id))
      |> Enum.reject(&Enum.member?(known_addrs, &1.address))
      |> Map.new(&({&1.address, &1}))

    announced_addrs = peers
    |> Enum.map(&({&1.ip, &1.port}))
    |> Enum.uniq()
    |> Enum.into([])

    all_peers = Map.merge(new_peers, d.peers)
    {dec_failcount, keep_failcount} = Map.split(all_peers, announced_addrs)

    all_updated_peers = dec_failcount
    |> Enum.map(fn {addr, p} -> {addr, Peer.dec_fail_count(p)} end)
    |> Map.new()
    |> Map.merge(keep_failcount)

    peer_addresses = all_updated_peers
    |> Enum.reject(fn {_addr, p} -> p.remote_peer_id == nil end)
    |> Map.new(fn {addr, p} -> {p.remote_peer_id, addr} end)

    d = d
    |> Map.put(:peers, all_updated_peers)
    |> Map.put(:peer_addresses, peer_addresses)
  end

  def delegate_message(d, remote_peer_id, msg)
       when is_peer_id(remote_peer_id) do

    with {:ok, peer} <- get_connected_peer(d, remote_peer_id),
         {peer, responses} <- Peer.recv(peer, msg),
         d = Map.update(d, :peers, Map.new(), &Map.put(&1, peer.address, peer)) do
      {d, Enum.map(responses, fn r -> {:btp_send, remote_peer_id, r} end)}
    else
      _ -> {d, []}
    end
  end

  defp get_connected_peer(d, remote_peer_id)
       when is_peer_id(remote_peer_id) do
    with {:ok, address} <- Map.fetch(d.peer_addresses, remote_peer_id) do
      Map.fetch(d.peers, address)
    else
      _ -> {:error, :peer_not_found}
    end
  end


  defp peer(d, peer_id, peer_address)
       when is_peer_id(peer_id) do
    Peer.new(peer_address, d.peer_id, d.info_hash, self())
    |> Map.put(:remote_peer_id, peer_id)
  end

  def handle_connect(d, peer_id, address)
      when is_peer_id(peer_id) do
    _ = Logger.debug("Handling connection success to #{inspect(address)}")
    d
    |> Map.update(:peers, Map.new(), &Map.put(&1, address, peer(d, peer_id, address)))
    |> Map.update(:peer_addresses, Map.new(), &Map.put(&1, peer_id, address))
  end
end
