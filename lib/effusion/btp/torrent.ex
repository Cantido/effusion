defmodule Effusion.BTP.Torrent do
  require Logger
  alias Effusion.BTP.Block

  @moduledoc """
  Functions for assembling the file that results from a torrent download.
  """

  @doc """
  Create a map that describes a torrent download.
  """
  def new(info = %{piece_length: _, pieces: _}) do
    %{info: info, blocks: MapSet.new()}
  end

  @doc """
  Get the bitfield value representing the torrent's finished pieces.

  This bitfield includes both in-memory as well as on-disk pieces.
  """
  def bitfield(torrent) do
    written = finished_pieces(torrent)
    cached = Enum.map(pieces(torrent), fn p -> p.index end) |> IntSet.new()

    IntSet.union(written, cached)
  end

  @doc """
  Add a block of data to `torrent`.

  If the addition of the block finishes a piece,
  the piece will then be verified and moved to the `:pieces` set.
  """
  def add_block(torrent = %{info: %{piece_length: piece_length}}, block = %{data: data})
  when is_integer(piece_length)
   and 0 <= piece_length
   and byte_size(data) <= piece_length do
    {finished, unfinished} =
      blocks(torrent)
      |> reduce_blocks(block)
      |> split_finished(torrent.info)

    finished = finished
      |> strip_offsets()
      |> verify_all(torrent.info)

    pieces1 = pieces(torrent) |> MapSet.union(finished)

    torrent
    |> Map.put(:blocks, unfinished)
    |> Map.put(:pieces, pieces1)
  end

  @doc """
  Get the set of blocks cached by this torrent.
  """
  def blocks(torrent) do
    Map.get(torrent, :blocks, MapSet.new())
  end

  @doc """
  Get the pieces that have been verified and written to disk.
  """
  def finished_pieces(torrent) do
    Map.get(torrent, :written, IntSet.new())
  end

  @doc """
  Get the pieces that have been verified but not yet written to disk.
  """
  def pieces(torrent) do
    Map.get(torrent, :pieces, MapSet.new())
  end

  @doc """
  Get the pieces that have been verified and written to disk.
  """
  def written(torrent) do
    Map.get(torrent, :written, IntSet.new())
  end

  @doc """
  Check if the torrent has cached or written all of the pieces it needs to be complete.
  """
  def done?(torrent) do
    (pieces(torrent) |> Enum.count()) == Enum.count(torrent.info.pieces)
  end

  @doc """
  Get the number of bytes that have been added to this torrent.

  This includes bytes in blocks that have not yet been verified.
  """
  def bytes_completed(torrent) do
    bytes_received(torrent) + bytes_written(torrent) + bytes_in_blocks(torrent)
  end

  defp bytes_received(torrent) do
    info = torrent.info
    pieces = pieces(torrent)

    last_piece_size = rem(info.length, info.piece_length)
    last_piece_index = Enum.count(info.pieces) - 1
    has_last_piece = Enum.any? pieces, fn p -> p.index == last_piece_index end

    naive_size = Enum.count(pieces) * torrent.info.piece_length

    if has_last_piece do
      naive_size - torrent.info.piece_length + last_piece_size
    else
      naive_size
    end
  end

  defp bytes_written(torrent) do
    info = torrent.info
    pieces = Map.get(torrent, :written, IntSet.new())

    last_piece_size = rem(info.length, info.piece_length)
    last_piece_index = Enum.count(info.pieces) - 1
    has_last_piece = Enum.member? pieces, last_piece_index

    naive_size = Enum.count(pieces) * torrent.info.piece_length

    if has_last_piece do
      naive_size - torrent.info.piece_length + last_piece_size
    else
      naive_size
    end
  end

  defp bytes_in_blocks(torrent) do
    blocks(torrent)
    |> Enum.reduce(0, fn b, acc -> acc + byte_size(b.data) end)
  end

  @doc """
  Get the number of bytes still necessary for this download to be finished.
  """
  def bytes_left(torrent) do
    if done?(torrent) do
      0
    else
      torrent.info.length - bytes_completed(torrent)
    end
  end

  @doc """
  Remove a piece from the torrent.
  """
  def remove_piece(torrent, piece) do
    pieces1 = pieces(torrent) |> MapSet.delete(piece)

    Map.put(torrent, :pieces, pieces1)
  end

  defp reduce_blocks(blocks, block) do
    adjacent_block = Enum.find(blocks, fn b -> Block.adjacent?(b, block) end)
    if adjacent_block == nil do
      MapSet.put(blocks, block)
    else
      blocks
      |> MapSet.delete(adjacent_block)
      |> MapSet.put(Block.merge(adjacent_block, block))
    end
  end

  defp split_finished(blocks, info) do
    finished = find_finished(blocks, info)
    unfinished = MapSet.difference(blocks, finished)

    {finished, unfinished}
  end

  defp find_finished(blocks, %{piece_length: target_size, length: file_size, pieces: pieces}) do
    last_piece_index = Enum.count(pieces) - 1
    last_piece_size = rem file_size, target_size

    finished = find_finished_whole_pieces(blocks, target_size)
    finished_last = find_finished_last_piece(blocks, last_piece_index, last_piece_size)

    if finished_last == nil do
      finished
    else
      MapSet.put(finished, finished_last)
    end
  end

  defp find_finished_whole_pieces(blocks, target_size) do
    finished? = &Block.finished?(&1, target_size)

    Enum.filter(blocks, finished?) |> MapSet.new()
  end

  defp find_finished_last_piece(blocks, last_piece_index, last_piece_size) do
    Enum.find(blocks, fn b ->
      b.index == last_piece_index and byte_size(b.data) == last_piece_size
    end)
  end

  defp strip_offsets(pieces) do
    Enum.map(pieces, fn p ->
      Block.to_piece(p)
    end)
    |> MapSet.new()
  end

  defp verify_all(pieces, info) do
    Enum.filter(pieces, &Block.correct_hash?(&1, info)) |> MapSet.new()
  end
end
