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

  def add_data(%__MODULE__{blocks: blocks} = piece, offset, data) do
    %__MODULE__{piece | blocks: [%Block{offset: offset, data: data} | blocks]}
  end

  def size(%__MODULE__{blocks: blocks}) do
    Enum.map(blocks, &Block.size/1)
    |> Enum.sum()
  end

  def data(%__MODULE__{blocks: blocks}) do
    Enum.sort_by(blocks, & &1.offset, :asc)
    |> Enum.map(& &1.data)
    |> Enum.join()
  end

  def finished?(%__MODULE__{} = piece) do
    size(piece) == piece.expected_size and
      Effusion.Hash.calc(data(piece)) == piece.expected_hash
  end
end
