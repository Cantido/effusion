defmodule Effusion.BTP.Torrent do
  require Logger
  alias Effusion.BTP.Block
  alias Effusion.Hash

  def new(info) do
    %{info: info, blocks: MapSet.new()}
  end

  def add_block(torrent, block) do
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

  def blocks(torrent) do
    Map.get(torrent, :blocks, MapSet.new())
  end

  def finished_pieces(torrent) do
    Map.get(torrent, :written, IntSet.new())
  end

  def pieces(torrent) do
    Map.get(torrent, :pieces, MapSet.new())
  end

  def written(torrent) do
    Map.get(torrent, :written, IntSet.new())
  end

  def done?(torrent) do
    (pieces(torrent) |> Enum.count()) == Enum.count(torrent.info.pieces)
  end

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
    Enum.filter(pieces, &correct_hash?(&1, info)) |> MapSet.new()
  end

  defp correct_hash?(piece, info) do
    expected_hash = Enum.at(info.pieces, piece.index)
    Hash.matches?(expected_hash, piece.data)
  end

  def write_to(torrent, file) do
    write_pieces(torrent, file, Enum.to_list(pieces(torrent)))
  end

  defp write_pieces(torrent, file, [piece | rest]) do
    {:ok, torrent} = write_piece(torrent, file, piece)

    write_pieces(torrent, file, rest)
  end

  defp write_pieces(torrent, _file, []) do
    {:ok, torrent}
  end

  def write_piece(torrent, file, %{index: i, data: d}) when is_binary(d) do
    piece_start_byte = i * torrent.info.piece_length
    _ = Logger.debug("writing #{inspect(d)} to #{inspect(file)}")
    :ok = :file.pwrite(file, {:bof, piece_start_byte}, [d])
    torrent = torrent
      |> Map.update(:written, IntSet.new(i), &IntSet.put(&1, i))
    {:ok, torrent}
  end
end
