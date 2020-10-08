defmodule Effusion.CQRS.EventHandlers.FileWriter do
  use Commanded.Event.Handler,
    application: Effusion.CQRS.Application,
    name: __MODULE__,
    consistency: :strong

  alias Effusion.CQRS.Events.{
    PieceHashSucceeded
  }
  alias Effusion.Range
  require Logger

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
      file_range = Range.poslen(file_start, f.length)
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
