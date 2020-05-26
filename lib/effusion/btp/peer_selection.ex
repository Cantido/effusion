defmodule Effusion.BTP.PeerSelection do
  alias Effusion.BTP.Peer
  alias Effusion.Repo
  import Ecto.Query

  @moduledoc """
  Selects which peers to connect to.
  """

  def select_lowest_failcount(info_hash, count) when is_integer(count) and count >= 0 do
    query =
      from peer in Peer,
      join: torrent in assoc(peer, :torrent),
      where: not peer.connected,
      where: torrent.info_hash == ^info_hash,
      order_by: [asc: :failcount],
      limit: ^count
    Repo.all(query)
  end
end
