defmodule Effusion.BTP.PeerSelection do
  alias Effusion.BTP.Peer
  alias Effusion.Repo
  import Ecto.Query

  @moduledoc """
  Selects which peers to connect to.
  """

  def select_lowest_failcount(info_hash, count, excluded_peer_ids \\ [])

  def select_lowest_failcount(info_hash, count, excluded_peer_ids) when is_integer(count) and count > 0 do
    query =
      from peer in Peer,
      join: torrent in assoc(peer, :torrent),
      where: peer.peer_id not in ^excluded_peer_ids,
      where: torrent.info_hash == ^info_hash,
      order_by: [asc: :failcount],
      limit: ^count
    Repo.all(query)
  end

  def select_lowest_failcount(_info_hash, 0, _excluded_peer_ids) do
    []
  end
end
