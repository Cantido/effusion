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

  def byte_mappings(info) do
    info.files
    |> Enum.with_index()
    |> Enum.map(fn {f, fi} ->
      file_start = first_byte_index(info, fi)
      file_range = Range.poslen(file_start, f.length)
      {f, file_range}
    end)
  end

  defp first_byte_index(_info, 0), do: 0

  defp first_byte_index(info, file_index) when file_index > 0 do
    info.files
    |> Enum.slice(0..(file_index - 1))
    |> Enum.map(& &1.length)
    |> Enum.sum()
  end
end
