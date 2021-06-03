defmodule Effusion.Metadata do
  def piece_size(info, index) do
    expected_piece_count = Enum.count(info.pieces)

    if index == expected_piece_count - 1 do
      info.length - (expected_piece_count - 1) * info.piece_length
    else
      info.piece_length
    end
  end
end
