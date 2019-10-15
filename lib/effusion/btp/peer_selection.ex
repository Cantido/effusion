defmodule Effusion.BTP.PeerSelection do
  def select_peer(self_id, peers, closed_connections) do
    disconnected_addresses = Enum.map(closed_connections, fn c -> c.address end) |> MapSet.new()

    disconnected_ids =
      Enum.map(closed_connections, fn c -> c.remote_peer_id end) |> MapSet.new()

    eligible_peers =
      peers
      |> Enum.reject(fn p ->
        peer_id = Map.get(p, :remote_peer_id)

        reject_peer_id? =
          peer_id != nil && (peer_id == self_id || MapSet.member?(disconnected_ids, peer_id))

        reject_address? = MapSet.member?(disconnected_addresses, p.address)

        reject_address? || reject_peer_id?
      end)

    if Enum.empty?(eligible_peers) do
      nil
    else
      Enum.random(eligible_peers)
    end
  end
end