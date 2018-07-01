defmodule Effusion.BTP.PieceSelection do
  require Logger
  alias Effusion.BTP.Block
  alias Effusion.BTP.Torrent

  @moduledoc """
  Strategies for selecting which pieces of a torrent to download.
  """

  defguardp is_size(x) when is_integer(x) and x > 0

  @doc """
  Get the next block to request, or `nil` if nothing can be requested.
  """
  def next_block(torrent, peers, block_size) do
    if Torrent.all_present?(torrent) do
      nil
    else
      poss = possible_requests(torrent, peers, block_size)

      if Enum.any?(poss) do
        Enum.random(poss)
      else
        nil
      end
    end
  end

  # Returns a set of ID-block pairs, of all blocks that can be requested, and from what peers
  defp possible_requests(torrent, peers, block_size) do
    we_have = Torrent.bitfield(torrent)
    info = torrent.info
    peers
    # select peers that have pieces we need
    |> Enum.map(fn p -> {p.remote_peer_id, p.has} end)
    |> Enum.map(fn {id, has} -> {id, IntSet.difference(has, we_have)} end)
    |> Enum.reject(fn {id, has} -> Enum.empty?(has) end)
    # Split out pairs of peer IDs and single pieces
    |> Enum.flat_map(fn {id, has} -> for p <- has, do: {id, p} end)
    |> Enum.map(fn {id, p} -> {id, Block.id(p, 0, piece_size(p, info))} end)
    # Split ID-piece pairs into many ID-block pairs
    |> Enum.flat_map(fn {id, p} -> for b <- Block.split(p, block_size), do: {id, b} end)
    # Reject ones we have
    |> Enum.reject(fn {id, b} -> Torrent.has_block?(torrent, b) end)
  end

  defp piece_size(index, info) do
    {whole_piece_count, last_piece_size} = divrem(info.length, info.piece_length)
    last_piece_index = whole_piece_count - 1

    if(index == last_piece_index) do
      last_piece_size
    else
      info.piece_length
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
