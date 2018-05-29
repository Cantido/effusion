defmodule Effusion.BTP.Block do
  defguard is_index(i) when is_integer(i) and i >= 0
  defguard is_size(x) when is_integer (x) and x > 0

  def new(i, o, d) when is_index(i) and is_index(o) and is_binary(d) do
    %{index: i, offset: o, data: d}
  end

  def new(i, o, s) when is_index(i) and is_index(o) and is_size(s) do
    %{index: i, offset: o, size: s}
  end

  def sequential?(%{index: i1, offset: o1, data: d1}, %{index: i2, offset: o2}) do
    (i1 == i2) and (o1 + byte_size(d1) == o2)
  end

  def adjacent?(b1, b2) do
    sequential?(b1, b2) or sequential?(b2, b1)
  end

  def merge(b1 = %{index: i1, offset: o1, data: d1}, b2 = %{index: i2, offset: o2, data: d2}) do
    cond do
      sequential?(b1, b2) ->
        %{
          index: i1,
          offset: o1,
          data: d1 <> d2
        }
      sequential?(b2, b1) ->
        %{
          index: i2,
          offset: o2,
          data: d2 <> d1
        }
    end
  end

  def finished?(block, target_size) do
    size(block) == target_size
  end

  def size(%{data: d}) do
    byte_size(d)
  end

  def size(%{size: s}) do
    s
  end

  def to_piece(%{index: i, offset: _, data: d}) do
    %{index: i, data: d}
  end

  def split(%{index: index, offset: 0, size: piece_size} = piece, block_size) when is_index(index) and is_size(piece_size) and is_size(block_size) do
    {whole_block_count, last_block_size} = divrem(piece.size, block_size)
    whole_block_indices = 0..(whole_block_count - 1)

    whole_blocks =
      if whole_block_count > 0 do
        for b_i <- whole_block_indices,
            offset = b_i * block_size,
            into: MapSet.new()
        do
          new(piece.index, offset, block_size)
        end
      else
        MapSet.new()
      end

    if last_block_size == 0 do
      whole_blocks
    else
      last_block_index = whole_block_count
      last_block_offset = last_block_index * block_size
      last_block = new(piece.index, last_block_offset, last_block_size)
      MapSet.put(whole_blocks, last_block)
    end
  end

  defp divrem(a, b) when is_integer(a) and is_integer(b) and b != 0 do
    {div(a, b), rem(a, b)}
  end
end
