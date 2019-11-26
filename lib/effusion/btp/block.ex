defmodule Effusion.BTP.Block do
  alias Effusion.Hash
  import Effusion.Math

  @moduledoc """
  A chunk of data for a download.

  A *block* of data is distinct from a *piece*.
  A piece is a chunk of data that is `piece_length` long,
  and has a 20-byte SHA-1 hash in the torrent's `pieces` array.
  Blocks can be accumulated into pieces, and both have a piece index and data.
  Blocks will also have an `:offset` value,
  indicating the data's offset from the start of the piece with index `:index`.
  """

  @enforce_keys [:index, :offset, :size]
  defstruct [:index, :offset, :size, data: <<>>]

  defguardp is_index(i) when is_integer(i) and i >= 0
  defguardp is_size(x) when is_integer(x) and x > 0

  @doc """
  Make a data structure that represents a block.

  This object carries no download data.
  """
  def id(i, o, s) when is_index(i) and is_index(o) and is_size(s) do
    %__MODULE__{index: i, offset: o, size: s}
  end

  def id(%{index: i, offset: o, size: s}) do
    %__MODULE__{index: i, offset: o, size: s}
  end

  def id(%{index: i, offset: o, data: d}) do
    %__MODULE__{index: i, offset: o, size: byte_size(d)}
  end

  @doc """
  Make a data structure for a block containing some data.
  """
  def new(i, o, d) when is_index(i) and is_index(o) and is_binary(d) do
    %__MODULE__{index: i, offset: o, size: byte_size(d), data: d}
  end

  @doc """
  Check if the first block is followed immedately by the second block,
  if they are in the same piece.
  """
  def sequential?(%{index: i1, offset: o1, data: d1}, %{index: i2, offset: o2}) do
    i1 == i2 and o1 + byte_size(d1) == o2
  end

  @doc """
  Check if the two blocks are contiguous, with either b1 first or b2 first.
  """
  def adjacent?(b1, b2) do
    sequential?(b1, b2) or sequential?(b2, b1)
  end

  @doc """
  Combine two contiguous blocks in the same piece into one larger block.
  """
  def merge(b1 = %{index: i1, offset: o1, data: d1}, b2 = %{index: i2, offset: o2, data: d2}) do
    cond do
      sequential?(b1, b2) ->
        %__MODULE__{
          index: i1,
          offset: o1,
          size: byte_size(d1 <> d2),
          data: d1 <> d2
        }

      sequential?(b2, b1) ->
        %__MODULE__{
          index: i2,
          offset: o2,
          size: byte_size(d2 <> d1),
          data: d2 <> d1
        }

      true ->
        {:error, :blocks_not_contiguous}
    end
  end

  @doc """
  Check if a block is a requisite size.
  """
  def finished?(block, target_size) do
    size(block) == target_size
  end

  @doc """
  Get the size of the data in, or identified by, a block.
  """
  def size(block)

  def size(%{data: d}) do
    byte_size(d)
  end

  def size(%{size: s}) do
    s
  end

  @doc """
  Drop the `:offset` value from a block, indicating that it is now a whole piece.
  """
  def to_piece(%{index: i, offset: _, data: d}) do
    %{index: i, data: d}
  end

  @doc """
  Split a piece into many blocks of a certain size.
  """
  def split(%{index: index, offset: 0, size: piece_size} = piece, block_size)
      when is_index(index) and is_size(piece_size) and is_size(block_size) do
    split_stream(piece, block_size) |> MapSet.new()
  end

  def split_stream(%{index: index, offset: 0, size: piece_size} = piece, block_size)
      when is_index(index) and is_size(piece_size) and is_size(block_size) do
    {whole_block_count, last_block_size} = divrem(piece.size, block_size)
    whole_block_indices = 0..(whole_block_count - 1)

    whole_blocks =
      if whole_block_count > 0 do
        Stream.map(whole_block_indices, fn b_i ->
          offset = b_i * block_size
          id(piece.index, offset, block_size)
        end)
      else
        MapSet.new()
      end

    if last_block_size == 0 do
      whole_blocks
    else
      last_block_index = whole_block_count
      last_block_offset = last_block_index * block_size
      last_block = id(piece.index, last_block_offset, last_block_size)
      Stream.concat(whole_blocks, [last_block])
    end
  end

  @doc """
  Check that the block's data matches the SHA-1 hash in a metadata map.
  """
  def correct_hash?(block, info) do
    expected_hash = Enum.at(info.pieces, block.index)
    Hash.matches?(expected_hash, block.data)
  end
end
