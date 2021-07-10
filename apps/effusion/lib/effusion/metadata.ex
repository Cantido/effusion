defmodule Effusion.Metadata do
  alias Effusion.Range

  def piece_size(info, index) do
    expected_piece_count = Enum.count(info.pieces)

    if index == expected_piece_count - 1 do
      info.length - (expected_piece_count - 1) * info.piece_length
    else
      info.piece_length
    end
  end

  def piece_byte_index(info, index) do
    index * info.piece_length
  end

  def piece_byte_range(info, index) do
    Effusion.Range.from_poslen(piece_byte_index(info, index), info.piece_length)
  end

  def file_byte_ranges(info = %Metatorrent.SingleFileInfo{}) do
    [{info.name, 0..(info.length - 1)}]
  end

  def file_byte_ranges(info = %Metatorrent.MultiFileInfo{}) do
    info.files
    |> Enum.with_index()
    |> Enum.map(fn {f, fi} ->
      file_start = first_byte_index(info.files, fi)
      file_range = Range.from_poslen(file_start, f.length)
      {Path.join(info.name, f.path), file_range}
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
