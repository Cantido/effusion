defmodule Effusion.BTP.Request do
  use Ecto.Schema
  alias Effusion.BTP.{Block, Peer, PeerPiece}
  alias Effusion.Repo
  import Ecto.Query

  schema "requests" do
    belongs_to :block, Block
    belongs_to :peer, Peer
  end

  @doc """
  Query for all pending requests for pieces in the given torrent.
  """
  def pending_query(info_hash) do
    from requests in __MODULE__,
      join: peer in assoc(requests, :peer),
      join: block in assoc(requests, :block),
      join: piece in assoc(block, :piece),
      join: torrent in assoc(piece, :torrent),
      where: torrent.info_hash == ^info_hash,
      select: {piece, block, peer}
  end

  def valid_requests_from_peer_query(info_hash, peer_id, count) do
    existing_requests = pending_query(info_hash)
    requests_to_make = from peer_pieces in PeerPiece,
                        join: piece in assoc(peer_pieces, :piece),
                        join: block in assoc(piece, :blocks),
                        join: peer in assoc(peer_pieces, :peer),
                        join: torrent in assoc(piece, :torrent),
                        where: torrent.info_hash == ^info_hash and peer.peer_id == ^peer_id,
                        where: is_nil(block.data),
                        except: ^existing_requests,
                        limit: ^count,
                        select: {piece, block, peer}
  end

  @doc """
  Cancel all requests for a block.

  Returns the peers that had requests for that block, minus the one it was from.
  """
  def cancel(block, from) do
    peer_ids_to_send_cancel = Repo.all(from request in __MODULE__,
                                        join: block in assoc(request, :block),
                                        join: piece in assoc(block, :piece),
                                        join: peer in assoc(request, :peer),
                                        where: piece.index == ^block.index
                                           and block.offset == ^block.offset
                                           and peer.peer_id != ^from,
                                        select: {peer.peer_id, piece.index, block.offset, block.size})


    Repo.delete_all(from request in __MODULE__,
                      join: block in assoc(request, :block),
                      join: piece in assoc(block, :piece),
                      join: peer in assoc(request, :peer),
                      where: piece.index == ^block.index
                         and block.offset == ^block.offset)

    peer_ids_to_send_cancel
  end
end
