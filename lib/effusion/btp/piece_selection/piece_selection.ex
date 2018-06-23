defmodule Effusion.BTP.PieceSelection do
  require Logger
  alias Effusion.BTP.Block
  alias Effusion.BTP.Torrent

  @moduledoc """
  Strategies for selecting which pieces of a torrent to download.
  """

  defguardp is_size(x) when is_integer(x) and x > 0

  @doc """
  Get the next block to request, or `nil` if the torrent is done.
  """
  def next_block(torrent, _peers, block_size) do
    info = torrent.info
    have_pieces = Torrent.bitfield(torrent)
    all_blocks = all_possible_blocks(info.length, info.piece_length, block_size)

    next_block = all_blocks
      |> Enum.reject(fn(b) -> Map.get(b, :index) in have_pieces end)
      |> Enum.take_random(1)
      |> Enum.at(0)

    case next_block do
      nil -> :done
      b -> b
    end
  end

  defp all_possible_blocks(file_size, whole_piece_size, block_size)  do
    file_size
    |> file_to_pieces(whole_piece_size)
    |> Enum.flat_map(&Block.split(&1, block_size))
  end

  defp file_to_pieces(total_size, piece_size) when is_size(total_size) and is_size(piece_size) do
    {whole_piece_count, last_piece_size} = divrem(total_size, piece_size)
    whole_piece_indices = 0..(whole_piece_count - 1)

    whole_pieces =
      for i <- whole_piece_indices,
          into: MapSet.new()
      do
        Block.id(i, 0, piece_size)
      end

    if last_piece_size == 0 do
      whole_pieces
    else
      last_piece_index = whole_piece_count
      last_piece = Block.id(last_piece_index, 0, last_piece_size)
      MapSet.put(whole_pieces, last_piece)
    end
  end

  defp divrem(a, b) when is_integer(a) and is_integer(b) and b != 0 do
    {div(a, b), rem(a, b)}
  end
end
