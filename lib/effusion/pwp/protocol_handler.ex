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
  @supported_extensions [:fast]

  def connect(address, info_hash, remote_peer_id) do
    # This is where we would make the uTP/TCP decision, once we support uTP.
    OutgoingHandler.connect({address, info_hash, remote_peer_id})
  end

  def disconnect(info_hash, remote_peer_id, reason) do
    OutgoingHandler.disconnect(info_hash, remote_peer_id, reason)
  end

  def get_handshake(info_hash) do
    {:handshake, @local_peer_id, info_hash, @supported_extensions}
  end

  def recv_handshake({:handshake, _remote_peer_id, info_hash, _extensions}) do
    case Registry.lookup(BTPHandlerRegistry, info_hash) do
      [{_pid, _hash}] -> :ok
      _ -> {:error, :torrent_not_found}
    end
  end

  def recv_handshake({:handshake, remote_peer_id, remote_info_hash, _extensions}, info_hash, expected_peer_id) do
    with :ok <- validate_info_hash(info_hash, remote_info_hash),
         :ok <- validate_peer_id(expected_peer_id, remote_peer_id) do
      :ok
    else
      err -> err
    end
  end

  defp validate_info_hash(local_info_hash, remote_info_hash) do
    if local_info_hash == remote_info_hash do
      :ok
    else
      {:error, {:mismatched_info_hash, [expected: local_info_hash, actual: remote_info_hash]}}
    end
  end

  defp validate_peer_id(expected_peer_id, remote_peer_id) do
    if expected_peer_id == nil or expected_peer_id == remote_peer_id do
      :ok
    else
      {:error, {:mismatched_peer_id, [expected: expected_peer_id, actual: remote_peer_id]}}
    end
  end

  def handle_connect(info_hash, peer_id, address, extensions) when is_hash(info_hash) and is_peer_id(peer_id) do
    {:ok, _pid} = ConnectionRegistry.register(info_hash, peer_id)
    Peer.insert(info_hash, peer_id, address, true, extensions)
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

  def handle_message(info_hash, from, {:reject, block}) when is_hash(info_hash) and is_peer_id(from) do
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

  def handle_message(info_hash, remote_peer_id, :have_all) when is_hash(info_hash) and is_peer_id(remote_peer_id) do
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

    Repo.insert_all(PeerPiece, peer_pieces)

    if !Pieces.all_present?(info_hash) do
      ConnectionRegistry.btp_send(info_hash, remote_peer_id, :interested)
    end

    :ok
  end

  def handle_message(info_hash, remote_peer_id, :have_none) when is_hash(info_hash) and is_peer_id(remote_peer_id) do
    :ok
  end

  def handle_message(info_hash, remote_peer_id, {:allowed_fast, _index}) when is_hash(info_hash) and is_peer_id(remote_peer_id) do
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
