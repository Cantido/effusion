defmodule Effusion.BTP.Block do
  alias Effusion.BTP.Piece
  alias Effusion.BTP.Request
  import Effusion.Math
  use Ecto.Schema

  @moduledoc """
  A chunk of data for a download.

  A *block* of data is distinct from a *piece*.
  A piece is a chunk of data that is `piece_length` long,
  and has a 20-byte SHA-1 hash in the torrent's `pieces` array.
  Blocks can be accumulated into pieces, and both have a piece index and data.
  Blocks will also have an `:offset` value,
  indicating the data's offset from the start of the piece with index `:index`.
  """

  schema "blocks" do
    belongs_to :piece, Piece
    field :offset, :integer, source: :position, null: false
    field :size, :integer, null: false
    field :data, :binary, null: true
    has_one :torrent, through: [:piece, :torrent]
    has_many :requests, Request
    has_many :requests_from, through: [:requests, :peer_id]
  end

  defguardp is_size(x) when is_integer(x) and x > 0

  @doc """
  Split a piece into many blocks of a certain size.
  """
  def split(piece, block_size) when is_size(block_size) do
    split_stream(piece, block_size) |> MapSet.new()
  end

  def split_stream(%{size: piece_size} = piece, block_size)
      when is_size(piece_size) and is_size(block_size) do
    {whole_block_count, last_block_size} = divrem(piece.size, block_size)
    whole_block_indices = 0..(whole_block_count - 1)

    whole_blocks =
      if whole_block_count > 0 do
        Stream.map(whole_block_indices, fn b_i ->
          offset = b_i * block_size
          %__MODULE__{piece: piece, offset: offset, size: block_size}
        end)
      else
        MapSet.new()
      end

    if last_block_size == 0 do
      whole_blocks
    else
      last_block_index = whole_block_count
      last_block_offset = last_block_index * block_size
      last_block = %__MODULE__{piece: piece, offset: last_block_offset, size: block_size}
      Stream.concat(whole_blocks, [last_block])
    end
  end
end
