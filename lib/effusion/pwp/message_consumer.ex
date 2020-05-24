defmodule Effusion.PWP.MessageConsumer do
  use GenStage
  alias Effusion.BTP.Peer
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Piece
  alias Effusion.BTP.PeerPiece
  alias Effusion.BTP.Request
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.Repo
  import Effusion.BTP.Peer
  import Effusion.Hash, only: [is_hash: 1]
  import Ecto.Query
  require Logger

  def start_link(_opts) do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    {:consumer, 0, [subscribe_to: [MessageProducer]]}
  end

  @impl true
  def handle_events(events, _from, state) do
    Enum.each(events, &handle_message/1)
    {:noreply, [], state}
  end

  def handle_message({info_hash, from, {:piece, block}}) do
    Request.cancel(block, from)
    |> Enum.uniq()
    |> Enum.each(fn {peer_id, index, offset, size} ->
      ConnectionRegistry.btp_send(info_hash, peer_id, {:cancel, index, offset, size})
    end)

    Pieces.add_block(info_hash, block)

    peer_request_query = from request in Request,
                         join: peer in assoc(request, :peer),
                         where: peer.peer_id == ^from
    peer_request_count = Repo.aggregate(peer_request_query, :count, :peer_id)

    max_requests = Application.get_env(:effusion, :max_requests_per_peer)

    if peer_request_count <= max_requests / 2 do
      next_request_from_peer(info_hash, from, max_requests)
    end
    :ok

  end

  def handle_message({info_hash, from, {:reject, block}}) when is_hash(info_hash) and is_peer_id(from) do
    :ok = Request.reject(info_hash, block, from)

    peer_request_query = from request in Request,
                         join: peer in assoc(request, :peer),
                         where: peer.peer_id == ^from
    peer_request_count = Repo.aggregate(peer_request_query, :count, :peer_id)

    if peer_request_count <= 0 do
      next_request_from_peer(info_hash, from, Application.get_env(:effusion, :max_requests_per_peer))
    end
    :ok
  end

  def handle_message({info_hash, remote_peer_id, {:bitfield, bitfield}}) when is_hash(info_hash) and is_peer_id(remote_peer_id) do
    peer = Repo.one!(from p in Peer, where: [peer_id: ^remote_peer_id])
    indicies = IntSet.new(bitfield) |> Enum.to_list()
    pieces_query = Piece.all_indicies_query(info_hash, indicies)
    pieces = Repo.all(pieces_query)
    peer_pieces = Enum.map(pieces, fn p ->
      %{
        peer_id: peer.id,
        piece_id: p.id
      }
    end)

    Repo.insert_all(PeerPiece, peer_pieces)

    peer_query = from peer in Peer,
                 join: torrent in assoc(peer, :torrent),
                 where: peer.peer_id == ^remote_peer_id,
                 where: torrent.info_hash == ^info_hash

    Repo.update_all(peer_query, set: [am_interested: true])

    if !Pieces.has_pieces?(info_hash, bitfield) do
      ConnectionRegistry.btp_send(info_hash, remote_peer_id, :interested)
    end

    :ok
  end

  def handle_message({info_hash, remote_peer_id, {:have, i}}) when is_hash(info_hash) and is_peer_id(remote_peer_id) do
    peer = from p in Peer, where: p.peer_id == ^remote_peer_id
    piece = from p in Piece,
             join: torrent in assoc(p, :torrent),
             where: torrent.info_hash == ^info_hash
                and p.index == ^i

    peer = Repo.one!(peer)
    piece = Repo.one!(piece)

    Repo.insert(%PeerPiece{
      peer: peer,
      piece: piece
    })

    peer_query = from peer in Peer,
                 join: torrent in assoc(peer, :torrent),
                 where: peer.peer_id == ^remote_peer_id,
                 where: torrent.info_hash == ^info_hash

    Repo.update_all(peer_query, set: [am_interested: true])

    if !Pieces.has_piece?(info_hash, i) do
      ConnectionRegistry.btp_send(info_hash, remote_peer_id, :interested)
    end
    :ok
  end

  def handle_message({info_hash, remote_peer_id, :have_all}) when is_hash(info_hash) and is_peer_id(remote_peer_id) do
    peer = Repo.one!(from p in Peer, where: [peer_id: ^remote_peer_id])

    pieces_query = from piece in Piece,
                   join: torrent in assoc(piece, :torrent),
                   where: torrent.info_hash == ^info_hash,
                   select: piece.id

    piece_dbids = Repo.all(pieces_query)
    peer_pieces = Enum.map(piece_dbids, fn piece_dbid ->
      %{
        peer_id: peer.id,
        piece_id: piece_dbid
      }
    end)

    Repo.insert_all(PeerPiece, peer_pieces, on_conflict: :nothing)

    peer_query = from peer in Peer,
                 join: torrent in assoc(peer, :torrent),
                 where: peer.peer_id == ^remote_peer_id,
                 where: torrent.info_hash == ^info_hash

    Repo.update_all(peer_query, set: [am_interested: true])

    if !Pieces.all_present?(info_hash) do
      ConnectionRegistry.btp_send(info_hash, remote_peer_id, :interested)
    end

    :ok
  end

  def handle_message({info_hash, remote_peer_id, :have_none}) when is_hash(info_hash) and is_peer_id(remote_peer_id) do
    :ok
  end

  def handle_message({info_hash, remote_peer_id, {:allowed_fast, _index}}) when is_hash(info_hash) and is_peer_id(remote_peer_id) do
    :ok
  end

  def handle_message({info_hash, remote_peer_id, :choke}) when is_hash(info_hash) and is_peer_id(remote_peer_id) do
    Repo.delete_all(from request in Request,
                      join: peer in assoc(request, :peer),
                      where: peer.peer_id == ^remote_peer_id)

    peer_query = from peer in Peer,
                 join: torrent in assoc(peer, :torrent),
                 where: peer.peer_id == ^remote_peer_id,
                 where: torrent.info_hash == ^info_hash

    Repo.update_all(peer_query, set: [peer_choking: true])

    :ok
  end

  def handle_message({info_hash, remote_peer_id, :unchoke}) when is_hash(info_hash) and is_peer_id(remote_peer_id) do
    peer_query = from peer in Peer,
                 join: torrent in assoc(peer, :torrent),
                 where: peer.peer_id == ^remote_peer_id,
                 where: torrent.info_hash == ^info_hash

    Repo.update_all(peer_query, set: [peer_choking: false])

    next_request_from_peer(info_hash, remote_peer_id, 100)
    :ok
  end

  def handle_message({info_hash, remote_peer_id, :interested}) when is_hash(info_hash) and is_peer_id(remote_peer_id) do
    peer_query = from peer in Peer,
                 join: torrent in assoc(peer, :torrent),
                 where: peer.peer_id == ^remote_peer_id,
                 where: torrent.info_hash == ^info_hash

    Repo.update_all(peer_query, set: [peer_interested: true])
    :ok
  end

  def handle_message({info_hash, remote_peer_id, :uninterested}) when is_hash(info_hash) and is_peer_id(remote_peer_id) do
    peer_query = from peer in Peer,
                 join: torrent in assoc(peer, :torrent),
                 where: peer.peer_id == ^remote_peer_id,
                 where: torrent.info_hash == ^info_hash

    Repo.update_all(peer_query, set: [peer_interested: false])
    :ok
  end

  defp next_request_from_peer(info_hash, peer_id, count) when is_hash(info_hash) do
    requests = Request.valid_requests_from_peer_query(info_hash, peer_id, count)
    |> Repo.all()

    requests_to_insert = Enum.map(requests, fn {_piece, block, peer} ->
      %{
        block_id: block.id,
        peer_id: peer.id
      }
    end)
    Repo.insert_all(Request, requests_to_insert)

    Enum.each(requests, fn {piece, block, peer} ->
      ConnectionRegistry.btp_send(info_hash, peer.peer_id, {:request, piece.index, block.offset, block.size})
    end)
    :ok
  end
end
