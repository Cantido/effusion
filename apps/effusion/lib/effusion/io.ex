defmodule Effusion.IO do
  alias Effusion.Range
  alias Effusion.Metadata
  require Logger

  def write_piece(data, index, info) do
    destdir = Application.fetch_env!(:effusion, :download_destination)

    split_bytes_to_files(info, index, data)
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

  def split_bytes_to_files(info = %Metatorrent.SingleFileInfo{}, index, data) do
    locbytes = {index * info.piece_length, data}

    %{info.name => locbytes}
  end

  def split_bytes_to_files(info = %Metatorrent.MultiFileInfo{}, index, data) do
    piece_range = Range.poslen(index * info.piece_length, byte_size(data))

    Metadata.byte_mappings(info)
    |> Enum.filter(fn {_f, file_range} ->
      Range.overlap?(file_range, piece_range)
    end)
    |> Enum.map(fn {f, file_range = file_start.._} ->
      poslen = Range.overlap_poslen(file_range, piece_range)
      file_data = :binary.part(data, poslen)

      overlap = Range.overlap(file_range, piece_range)
      file_offset.._ = Effusion.Range.shift(overlap, -file_start)

      {Path.join([info.name, f.path]), {file_offset, file_data}}
    end)
  end
end
