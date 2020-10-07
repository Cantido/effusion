defmodule Effusion.CQRS.EventHandlers.DbWriter do
  use Commanded.Event.Handler,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  alias Effusion.CQRS.Events.{
    BlockStored,
    TorrentAdded,
    PieceHashSucceeded
  }
  import Ecto.Query
  require Logger

  def handle(
    %TorrentAdded{} = event,
    _metadata
  ) do
    event
    |> Map.from_struct()
    |> Map.update!(:info_hash, &Effusion.Hash.decode/1)
    |> Effusion.BTP.Torrent.insert()
    :ok
  end

  def handle(
    %BlockStored{from: from, index: index, offset: offset} = event,
    _metadata
  ) do
    Logger.debug("**** CQRS is writing block #{index}-#{offset} to the DB")

    block = Map.take(event, [:info_hash, :index, :offset, :data])
    |> Map.update!(:info_hash, &Effusion.Hash.decode/1)

    Effusion.BTP.Request.cancel(block, from)
    |> Enum.uniq()
    |> Enum.each(fn {peer_id, index, offset, size} ->
      Effusion.PWP.ConnectionRegistry.btp_send(block.info_hash, peer_id, {:cancel, index, offset, size})
    end)

    Effusion.BTP.Block.put(block.info_hash, block)

    peer_request_query =
      from request in Effusion.BTP.Request,
      join: peer in assoc(request, :peer),
      where: peer.peer_id == ^from
    peer_request_count = Effusion.Repo.aggregate(peer_request_query, :count, :peer_id)

    max_requests = Application.get_env(:effusion, :max_requests_per_peer)

    if peer_request_count <= max_requests / 2 do
      Effusion.PWP.ProtocolHandler.next_request_from_peer(block.info_hash, from, max_requests)
    end

    :ok
  end

  def handle(
    %PieceHashSucceeded{info_hash: info_hash, index: index},
    _metadata
  ) do
    Logger.debug "**** CQRS is writing piece #{index} of #{info_hash}"
    info_hash = Effusion.Hash.decode(info_hash)

    piece =
      Effusion.BTP.Piece.get(info_hash, index)
      |> Effusion.Repo.one!()
      |> Effusion.BTP.Piece.verify()

    Effusion.IO.write_piece(piece)
    Effusion.BTP.Pieces.mark_piece_written(info_hash, index)

    :ok
  end
end
