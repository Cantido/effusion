defmodule Effusion.BTP.Pieces do
  require Logger
  alias Effusion.BTP.Block
  alias Effusion.BTP.Metainfo

  @moduledoc """
  Functions for assembling the file that results from a torrent download.
  """

  @doc """
  Create a map that describes a torrent download.
  """
  def new(info_hash) do
    %{
      info_hash: info_hash,
      info: Metainfo.get_meta(info_hash).info,
      blocks: MapSet.new()
    }
  end

  @doc """
  Get the bitfield value representing the torrent's finished pieces.

  This bitfield includes both in-memory as well as on-disk pieces.
  """
  def bitfield(torrent) do
    written = written(torrent)
    cached = Enum.map(verified(torrent), fn p -> p.index end) |> IntSet.new()

    IntSet.union(written, cached)
  end

  def has_block?(torrent, block) do
    i = block.index
    o = block.offset

    unfinished(torrent)
    |> Enum.filter(fn b -> b.index == i end)
    |> Enum.filter(fn b -> b.offset == o end)
    |> Enum.any?()
  end

  def has_piece?(torrent, index) when is_integer(index) and index > 0 do
    bitfield(torrent) |> Enum.member?(index)
  end

  def has_pieces?(torrent, bits) do
    we_have = bitfield(torrent)
    do_we_have = IntSet.new(bits)
    IntSet.difference(do_we_have, we_have)
    |> Enum.empty?()
  end

  @doc """
  Add a block of data to `torrent`.

  If the addition of the block finishes a piece,
  the piece will then be verified and moved to the `:pieces` set.
  """
  def add_block(torrent = %{info: %{piece_length: piece_length}}, block = %{data: data})
      when is_integer(piece_length) and 0 <= piece_length and byte_size(data) <= piece_length do
    {finished, unfinished} =
      unfinished(torrent)
      |> reduce_blocks(block)
      |> split_finished(torrent.info)

    verified =
      finished
      |> strip_offsets()
      |> verify_all(torrent.info)
      |> MapSet.union(verified(torrent))

    torrent
    |> Map.put(:unfinished, unfinished)
    |> Map.put(:verified, verified)
  end

  @doc """
  Get the set of blocks cached by this torrent.
  """
  def unfinished(torrent) do
    Map.get(torrent, :unfinished, MapSet.new())
  end

  def verified(torrent) do
    Map.get(torrent, :verified, MapSet.new())
  end

  def written(torrent) do
    Map.get(torrent, :written, IntSet.new())
  end

  @doc """
  Updates the torrent to mark all verified pieces as written,
  then returns the verified pieces.
  """
  def take_verified(torrent) do
    v = verified(torrent) |> Enum.to_list()
    t = mark_pieces_written(torrent, v)
    {v, t}
  end

  @doc """
  Add a block to a torrent, then take the verified pieces, marking them as written.
  """
  def add_block_and_take_verified(torrent, block) do
    torrent
    |> add_block(block)
    |> take_verified()
  end

  @doc """
  Returns `true` if all pieces of this torrent have been written to disk.
  """
  def all_written?(torrent) do
    written(torrent) |> Enum.count() == Enum.count(torrent.info.pieces)
  end

  @doc """
  Check if the torrent has cached or written all of the pieces it needs to be complete.
  """
  def all_present?(torrent) do
    in_memory_pieces = verified(torrent) |> Enum.count()
    on_disk_pieces = written(torrent) |> Enum.count()

    in_memory_pieces + on_disk_pieces == Enum.count(torrent.info.pieces)
  end

  @doc """
  Get the number of bytes that have been added to this torrent.

  This includes bytes in blocks that have not yet been verified.
  """
  def bytes_completed(torrent) do
    unfinished_bytes(torrent) + verified_bytes(torrent) + bytes_written(torrent)
  end

  defp unfinished_bytes(torrent) do
    torrent
    |> unfinished()
    |> Enum.map(&Map.get(&1, :data))
    |> Enum.map(&byte_size/1)
    |> Enum.sum()
  end

  defp verified_bytes(torrent) do
    torrent
    |> verified()
    |> Enum.map(&Map.get(&1, :data))
    |> Enum.map(&byte_size/1)
    |> Enum.sum()
  end

  defp bytes_written(torrent) do
    info = torrent.info
    pieces = written(torrent)

    last_piece_size = rem(info.length, info.piece_length)
    last_piece_index = Enum.count(info.pieces) - 1
    has_last_piece = Enum.member?(pieces, last_piece_index)

    naive_size = Enum.count(pieces) * torrent.info.piece_length

    if has_last_piece do
      naive_size - torrent.info.piece_length + last_piece_size
    else
      naive_size
    end
  end

  @doc """
  Get the number of bytes still necessary for this download to be finished.
  """
  def bytes_left(torrent) do
    if all_present?(torrent) do
      0
    else
      torrent.info.length - bytes_completed(torrent)
    end
  end

  def mark_piece_written(torrent, %{index: i}) do
    torrent
    |> Map.update(:written, IntSet.new(i), &IntSet.put(&1, i))
    |> remove_piece(i)
  end

  def mark_piece_written(torrent, i) when is_integer(i) do
    torrent
    |> Map.update(:written, IntSet.new(i), &IntSet.put(&1, i))
    |> remove_piece(i)
  end

  def mark_pieces_written(torrent, [piece | rest]) do
    torrent
    |> mark_piece_written(piece)
    |> mark_pieces_written(rest)
  end

  def mark_pieces_written(torrent, []), do: torrent

  defp remove_piece(torrent, index) do
    verified =
      torrent
      |> verified()
      |> Enum.reject(fn p ->
        p.index == index
      end)
      |> MapSet.new()

    Map.put(torrent, :verified, verified)
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
    last_piece_size = rem(file_size, target_size)

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
