defmodule Effusion.BTP.Pieces do
  require Logger
  alias Effusion.BTP.Piece
  alias Effusion.BTP.Block
  alias Effusion.BTP.Metainfo
  alias Effusion.Repo
  alias Effusion.Hash
  import Ecto.Query

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

  def has_block?(_torrent, block) do
    i = block.index
    o = block.offset

    Repo.exists?(from b in block,
                 join: p in assoc(b, :piece),
                 where: p.index == ^i,
                 where: b.offset == ^o,
                 where: not is_nil(b.data))
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
  def add_block(torrent = %{info: %{piece_length: piece_length}}, block = %{index: i, offset: o, data: data})
      when is_integer(piece_length) and 0 <= piece_length and byte_size(data) <= piece_length do

    Logger.debug("inserting block #{inspect block}")
    {:ok, _block} = Repo.one!(from b in Block,
                      join: p in assoc(b, :piece),
                      where: p.index == ^i,
                      where: b.offset == ^o)
    |> Ecto.Changeset.change(data: data)
    |> Repo.update()

    blocks_per_piece_query = from piece in Piece,
                              join: block in assoc(piece, :blocks),
                              group_by: piece.id,
                              select: {piece.id, count(block.id)}

    blocks_per_piece_with_data_query = from piece in Piece,
                                        join: block in assoc(piece, :blocks),
                                        where: not is_nil(block.data),
                                        group_by: piece.id,
                                        select: {piece.id, count(block.id)}

    pieces_with_all_blocks = Repo.all(blocks_per_piece_query |> intersect(^blocks_per_piece_with_data_query))

    pieces_with_all_blocks |> Enum.each(fn {piece_dbid, _block_count} ->
      data_query = from block in Block,
                    join: piece in assoc(block, :piece),
                    where: piece.id == ^piece_dbid,
                    order_by: block.offset,
                    select: block.data

      data_blocks = Repo.all(data_query)

      piece_data = Enum.reduce(data_blocks, <<>>, fn data, bin ->
        bin <> data
      end)

      piece_expected_hash = Repo.one(from piece in Piece,
                                      where: piece.id == ^piece_dbid,
                                      select: piece.hash)

      piece_actual_hash = Hash.calc(piece_data)

      if piece_expected_hash == piece_actual_hash do
        Logger.debug("Piece verified! dbid: #{piece_dbid}")

        {:ok, _} = Repo.get(Piece, piece_dbid)
        |> Ecto.Changeset.change(verified: true)
        |> Repo.update()
      else
        Logger.warn("Piece failed verificiation! dbid: #{piece_dbid}")
        blocks_query = from block in Block,
                      join: piece in assoc(block, :piece),
                      where: piece.id == ^piece_dbid
        Repo.update_all(blocks_query, set: [data: nil])
      end
    end)
    torrent
  end

  @doc """
  Get the set of blocks cached by this torrent.
  """
  def unfinished(torrent) do
    unfinished_piece_blocks_query = from block in Block,
                                    join: piece in assoc(block, :piece),
                                    where: not piece.verified,
                                    where: not is_nil(block.data)

    Repo.all(unfinished_piece_blocks_query)
  end

  def verified(torrent) do
    verified_piece_blocks_query = from block in Block,
                                    join: piece in assoc(block, :piece),
                                    where: piece.verified,
                                    where: not piece.announced,
                                    select: %{index: piece.index, id: piece.id}

    Repo.all(verified_piece_blocks_query)
  end

  def written(torrent) do
    written_piece_blocks_query = from block in Block,
                                    join: piece in assoc(block, :piece),
                                    where: piece.written,
                                    select: piece.index

    Repo.all(written_piece_blocks_query) |> IntSet.new()
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
    count = Repo.one(from piece in Piece,
              where: piece.verified,
              select: sum(piece.size))
    if is_integer(count) do
      count
    else
      0
    end
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
    mark_piece_written(torrent, i)
  end

  def mark_piece_written(torrent, i) when is_integer(i) do
    Repo.one!(from piece in Piece,
               join: torrent in assoc(piece, :torrent),
               where: torrent.info_hash == ^torrent.info_hash,
               where: piece.index == ^i)
    |> Ecto.Changeset.change([written: true])
    |> Repo.update()

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
end
