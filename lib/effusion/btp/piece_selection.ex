defmodule Effusion.BTP.PieceSelection do
  require Logger

  defguard is_index(i) when is_integer(i) and i >= 0
  defguard is_size(x) when is_integer (x) and x > 0

  def next_block(info, have_pieces, block_size) do
    all_blocks = all_possible_blocks(info.length, info.piece_length, block_size)

    all_blocks
      |> Enum.reject(fn(b) -> Map.get(b, :index) in have_pieces end)
      |> hd()
  end

  def all_possible_blocks(file_size, whole_piece_size, block_size)  do
    file_size
    |> file_to_pieces(whole_piece_size)
    |> Enum.flat_map(&piece_to_blocks(&1, block_size))
  end

  defp file_to_pieces(total_size, piece_size) when is_size(total_size) and is_size(piece_size) do
    {whole_piece_count, last_piece_size} = divrem(total_size, piece_size)
    whole_piece_indices = 0..(whole_piece_count - 1)

    whole_pieces =
      for i <- whole_piece_indices,
          into: MapSet.new()
      do
        %{index: i, size: piece_size}
      end

    if last_piece_size == 0 do
      whole_pieces
    else
      last_piece_index = whole_piece_count
      last_piece = %{index: last_piece_index, size: last_piece_size}
      MapSet.put(whole_pieces, last_piece)
    end
  end

  defp piece_to_blocks(%{index: index, size: piece_size} = piece, block_size) when is_index(index) and is_size(piece_size) and is_size(block_size) do
    Logger.info "breaking piece #{inspect(piece)} into blocks of size #{block_size}"

    {whole_block_count, last_block_size} = divrem(piece.size, block_size)
    whole_block_indices = 0..(whole_block_count - 1)

    Logger.info "We will have #{whole_block_count} whole blocks, with a last block of size #{last_block_size}"

    whole_blocks =
      if whole_block_count > 0 do
        for b_i <- whole_block_indices,
            offset = b_i * block_size,
            into: MapSet.new()
        do
          block_id(piece.index, offset, block_size)
        end
      else
        MapSet.new()
      end
    Logger.info "got whole blocks: #{inspect(whole_blocks)}"

    if last_block_size == 0 do
      whole_blocks
    else
      last_block_index = whole_block_count
      last_block_offset = last_block_index * block_size
      last_block = block_id(piece.index, last_block_offset, last_block_size)
      MapSet.put(whole_blocks, last_block)
    end
  end

  defp divrem(a, b) when is_integer(a) and is_integer(b) and b != 0 do
    {div(a, b), rem(a, b)}
  end

  defp block_id(i, o, s) when is_index(i) and is_index(o) and is_size(s) do
    %{index: i, offset: o, size: s}
  end
end
