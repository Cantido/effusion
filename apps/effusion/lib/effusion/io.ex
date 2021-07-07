defmodule Effusion.IO do
  alias Effusion.Range
  require Logger

  def write_piece(data, index, info) do

    files = Map.get(info, :files, [])
    destdir = Application.fetch_env!(:effusion, :download_destination)

    split_bytes_to_files(files, info.name, info.piece_length, index, data)
    |> Enum.group_by(fn {path, _} -> path end, fn {_, locbytes} -> locbytes end)
    |> Enum.each(fn {rel_path, locbytes} ->
      path = Path.join(destdir, rel_path)

      with :ok <- File.mkdir_p(Path.dirname(path)),
          {:ok, device} <- File.open(path, [:read, :write]),
          :ok <- :file.pwrite(device, locbytes) do
        File.close(device)
      end
    end)

    :ok
  end

  def split_bytes_to_files([], name, piece_length, index, data) do
    locbytes = {index * piece_length, data}

    %{name => locbytes}
  end

  def split_bytes_to_files(files, name, piece_length, index, data) when is_list(files) do
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

      {Path.join([name, f.path]), {file_offset, file_data}}
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
