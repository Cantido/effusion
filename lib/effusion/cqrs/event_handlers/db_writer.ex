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
  alias Effusion.Range
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
    files = Map.get(info, :files, [])

    # TODO: this is only valid for single-file torrents
    destdir = Application.fetch_env!(:effusion, :download_destination)

    split_bytes_to_files(destdir, files, info.name, info.piece_length, index, data)
    |> Enum.group_by(fn {path, _} -> path end, fn {_, locbytes} -> locbytes end)
    |> Enum.each(fn {path, locbytes} ->
      with :ok <- File.mkdir_p(Path.dirname(path)),
           {:ok, device} <- File.open(path, [:read, :write]),
           :ok <- :file.pwrite(device, locbytes) do
        File.close(device)
      end
    end)

    :ok
  end

  def split_bytes_to_files(destdir, [], name, piece_length, index, data) do
    path = Path.join(destdir, name)
    locbytes = {index * piece_length, data}

    %{path => locbytes}
  end

  def split_bytes_to_files(destdir, files, name, piece_length, index, data) when is_list(files) do
    piece_range = Range.poslen(index * piece_length, byte_size(data))

    files
    |> Enum.with_index()
    |> Enum.map(fn {f, fi} ->
      file_start = first_byte_index(files, fi)
      file_range = Range.poslen(file_start, f.size)
      {f, file_range}
    end)
    |> Enum.filter(fn {_f, file_range} ->
      Range.overlap?(file_range, piece_range)
    end)
    |> Enum.map(fn {f, file_range = file_start.._} ->
      overlap = Range.overlap(file_range, piece_range)
      poslen = Range.overlap_poslen(file_range, piece_range)

      file_offset.._ = Effusion.Range.shift(overlap, -file_start)
      file_data = :binary.part(data, poslen)

      {Path.join([destdir, name, f.path]), {file_offset, file_data}}
    end)
  end

  defp first_byte_index(_torrent, 0), do: 0

  defp first_byte_index(files, file_index) when file_index > 0 do
    files
    |> Enum.slice(0..(file_index - 1))
    |> Enum.map(& &1.length)
    |> Enum.sum()
  end
end
