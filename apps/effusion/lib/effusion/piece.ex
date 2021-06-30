defmodule Effusion.Piece do
  alias Effusion.Block

  @enforce_keys [
    :index,
    :expected_hash,
    :expected_size
  ]
  defstruct [
    index: nil,
    expected_hash: nil,
    expected_size: nil,
    blocks: []
  ]

  def size(%__MODULE__{blocks: blocks}) do
    Enum.map(blocks, &Block.size/1)
    |> Enum.sum()
  end

  def data(%__MODULE__{blocks: blocks}) do
    Enum.sort_by(blocks, & &1.offset, :asc)
    |> Enum.map(& &1.data)
    |> Enum.join()
  end

  def get_block(%__MODULE__{blocks: blocks}, offset, size) do
    Enum.find(blocks, fn block ->
      block.offset == offset and block.size == size
    end)
  end

  @doc """
  Returns all blocks that are needed to finish this piece.
  Blocks are returned as `{offset, size}` tuples.

  Note: Blocks are not returned in any specific order.

  ## Examples

      iex> piece = %Effusion.Piece{index: 0, expected_hash: "12345678901234567890", expected_size: 2}
      ...> Effusion.Piece.needed_blocks(piece, 1) |> Enum.sort_by(&elem(&1, 0))
      [{0, 1}, {1, 1}]

  This function will never return blocks bigger than the piece itself, so if your block size is bigger than the piece,
  it will just return a block containing the entire piece.

      iex> piece = %Effusion.Piece{index: 0, expected_hash: "12345678901234567890", expected_size: 1}
      ...> Effusion.Piece.needed_blocks(piece, 16384) |> Enum.sort_by(&elem(&1, 0))
      [{0, 1}]

  It also handles the last block being a different size from the rest.

      iex> piece = %Effusion.Piece{index: 0, expected_hash: "12345678901234567890", expected_size: 5}
      ...> Effusion.Piece.needed_blocks(piece, 2) |> Enum.sort_by(&elem(&1, 0))
      [{0, 2}, {2, 2}, {4, 1}]

  """
  def needed_blocks(%__MODULE__{} = piece, block_size) do
    full_sized_blocks_count = div(piece.expected_size, block_size)

    full_sized_blocks =
      if full_sized_blocks_count == 0 do
        []
      else
        Enum.map(1..full_sized_blocks_count, fn block_number ->
          block_index = (block_number - 1)
          block_offset = block_index * block_size
          {block_offset, block_size}
        end)
      end

    last_block_offset = full_sized_blocks_count * block_size
    last_block_size = rem(piece.expected_size, block_size)
    last_block = {last_block_offset, last_block_size}

    if last_block_size == 0 do
      full_sized_blocks
    else
      [last_block | full_sized_blocks]
    end
    |> Enum.reject(fn {offset, _size} ->
      Enum.any?(piece.blocks, &(&1.offset == offset))
    end)
  end

  def finished?(%__MODULE__{} = piece) do
    size(piece) == piece.expected_size and
      Effusion.Hash.calc(data(piece)) == piece.expected_hash
  end

  def add_data(%__MODULE__{blocks: blocks} = piece, offset, data) do
    %__MODULE__{piece | blocks: [%Block{offset: offset, data: data} | blocks]}
  end
end
