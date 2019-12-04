defmodule Effusion.PWP.ProtocolHandler do
  alias Effusion.BTP.Peer
  alias Effusion.BTP.PeerSelection
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Piece
  alias Effusion.BTP.PeerPiece
  alias Effusion.BTP.Request
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.PWP.TCP.OutgoingHandler
  alias Effusion.Repo
  import Effusion.BTP.Peer
  import Effusion.Hash, only: [is_hash: 1]
  import Ecto.Query
  require Logger

  @moduledoc """
  Handles PWP messages.
  """

  @local_peer_id Application.get_env(:effusion, :peer_id)

  def connect(address, info_hash, remote_peer_id) do
    # This is where we would make the uTP/TCP decision, once we support uTP.
    OutgoingHandler.connect({address, info_hash, @local_peer_id, remote_peer_id})
  end

  def disconnect(info_hash, remote_peer_id, reason) do
    OutgoingHandler.disconnect(info_hash, remote_peer_id, reason)
  end

  def handle_connect(info_hash, peer_id, address) when is_hash(info_hash) and is_peer_id(peer_id) do
    {:ok, _pid} = ConnectionRegistry.register(info_hash, peer_id)
    Peer.insert(info_hash, peer_id, address, true)
    :ok
  end

  @doc """
  Handle a Peer Wire Protocol (PWP) message sent by a remote peer.
  """
  def handle_message(info_hash, from, {:piece, block}) when is_hash(info_hash) and is_peer_id(from) do
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

    if peer_request_count <= 0 do
      next_request_from_peer(info_hash, from, Application.get_env(:effusion, :max_requests_per_peer))
    end
    :ok
  end

  def handle_message(info_hash, remote_peer_id, {:bitfield, bitfield}) when is_hash(info_hash) and is_peer_id(remote_peer_id) do
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

    if !Pieces.has_pieces?(info_hash, bitfield) do
      ConnectionRegistry.btp_send(info_hash, remote_peer_id, :interested)
    end

    :ok
  end

  def handle_message(info_hash, remote_peer_id, {:have, i}) when is_hash(info_hash) and is_peer_id(remote_peer_id) do
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

    if !Pieces.has_piece?(info_hash, i) do
      ConnectionRegistry.btp_send(info_hash, remote_peer_id, :interested)
    end
    :ok
  end

  def handle_message(info_hash, remote_peer_id, :choke) when is_hash(info_hash) and is_peer_id(remote_peer_id) do
    Repo.delete_all(from request in Request,
                      join: peer in assoc(request, :peer),
                      where: peer.peer_id == ^remote_peer_id)

    :ok
  end

  def handle_message(info_hash, remote_peer_id, :unchoke) when is_hash(info_hash) and is_peer_id(remote_peer_id) do
    next_request_from_peer(info_hash, remote_peer_id, 100)
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

  def disconnect_all(info_hash) do
    ConnectionRegistry.disconnect_all(info_hash)
  end

  @doc """
  Handle a peer disconnection.
  """
  def handle_disconnect(info_hash, {ip, port}, reason) do
    PeerSelection.select_lowest_failcount(info_hash, 1)
        |> Enum.map(fn peer ->
          address = {peer.address.address, peer.port}
          connect(address, info_hash, peer.peer_id)
        end)

    disconnected_peer = Repo.one!(from peer in Peer,
                                  join: torrent in assoc(peer, :torrent),
                                  where: torrent.info_hash == ^info_hash,
                                  where: peer.address == ^%Postgrex.INET{address: ip},
                                  where: peer.port == ^port)
                        |> Peer.changeset()

    if reason != :normal do
      Repo.update(disconnected_peer, update: [inc: [failcount: 1], set: [connected: false]])
    else
      Repo.update(disconnected_peer, update: [set: [connected: false]])
    end
    :ok
  end
end
