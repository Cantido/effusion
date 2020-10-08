defmodule Effusion.CQRS.EventHandlers.DbWriter do
  use Commanded.Event.Handler,
    application: Effusion.CQRS.Application,
    name: __MODULE__,
    consistency: :strong

  alias Effusion.CQRS.Events.{
    BlockStored,
    TorrentAdded,
    DownloadStarted,
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
    %DownloadStarted{info_hash: info_hash},
    _metadata
  ) do
    Effusion.Hash.decode(info_hash)
    |> Effusion.BTP.Torrent.by_info_hash!()
    |> Effusion.BTP.Torrent.start(DateTime.utc_now())
    |> Effusion.Repo.update()
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
    %PieceHashSucceeded{info_hash: info_hash, index: index, data: data, info: info},
    _metadata
  ) do
    Logger.debug "**** CQRS is writing piece #{index} of #{info_hash}"
    info_hash = Effusion.Hash.decode(info_hash)

    # TODO: this is only valid for single-file torrents
    destdir = Application.fetch_env!(:effusion, :download_destination)
    destfile = Path.join(destdir, info.name)
    locbytes = [{index * info.piece_length, data}]

    with :ok <- File.mkdir_p(Path.dirname(destfile)),
         {:ok, device} <- File.open(destfile, [:read, :write]),
         :ok <- :file.pwrite(device, locbytes) do
      File.close(device)
    end

    :ok
  end
end
