defmodule Effusion.BTP.Pieces do
  require Logger
  alias Effusion.BTP.Block
  alias Effusion.BTP.Piece
  alias Effusion.Repo
  import Effusion.Hash, only: [is_hash: 1]
  import Ecto.Query

  @moduledoc """
  Functions for assembling the file that results from a torrent download.
  """

  @doc """
  Check if we have downloaded and verified the given piece.
  """
  def has_piece?(info_hash, index) when is_hash(info_hash) and is_integer(index) and index > 0 do
    Repo.exists?(
      from piece in Piece,
      join: torrent in assoc(piece, :torrent),
      where: torrent.info_hash == ^info_hash,
      where: piece.index == ^index,
      where: piece.verified
    )
  end

  @doc """
  Check if we have donwloaded and verified pieces at all the given indices.
  """
  def has_pieces?(info_hash, indices) when is_hash(info_hash) do
    Repo.exists?(
      from piece in Piece,
      join: torrent in assoc(piece, :torrent),
      where: torrent.info_hash == ^info_hash,
      where: piece.index in ^indices,
      where: piece.verified
    )
  end

  @doc """
  Verify all pieces for a given torrent.
  """
  def verify_all(info_hash) do
    blocks_per_piece_query =
      from piece in Piece,
      join: block in assoc(piece, :blocks),
      join: torrent in assoc(piece, :torrent),
      where: torrent.info_hash == ^info_hash,
      where: not piece.verified,
      group_by: piece.id,
      select: {piece.id, count(block.id)}

    blocks_per_piece_with_data_query =
      from piece in Piece,
      join: block in assoc(piece, :blocks),
      join: torrent in assoc(piece, :torrent),
      where: torrent.info_hash == ^info_hash,
      where: not is_nil(block.data),
      where: not piece.verified,
      group_by: piece.id,
      select: {piece.id, count(block.id)}

    pieces_with_all_blocks = Repo.all(blocks_per_piece_query |> intersect(^blocks_per_piece_with_data_query))

    pieces_with_all_blocks |> Enum.each(fn {piece_dbid, _block_count} ->
      data_query =
        from block in Block,
        join: piece in assoc(block, :piece),
        where: piece.id == ^piece_dbid,
        select: fragment(
          "digest(string_agg(?, '' ORDER BY ?), 'sha1')",
          block.data,
          block.offset
        )

      piece_actual_hash = Repo.one!(data_query)

      piece_expected_hash = Repo.one!(from piece in Piece,
                                      where: piece.id == ^piece_dbid,
                                      select: piece.hash)

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
  end

  @doc """
  Get the set of blocks cached by this torrent.
  """
  def unfinished(info_hash) when is_hash(info_hash) do
    unfinished_piece_blocks_query =
      from block in Block,
      join: piece in assoc(block, :piece),
      join: torrent in assoc(piece, :torrent),
      where: torrent.info_hash == ^info_hash,
      where: not piece.verified,
      where: not is_nil(block.data),
      select: block

    Repo.all(unfinished_piece_blocks_query)
  end

  def verified(info_hash) when is_hash(info_hash) do
    verify_all(info_hash)

    verified_piece_blocks_query =
      from block in Block,
      join: piece in assoc(block, :piece),
      join: torrent in assoc(piece, :torrent),
      where: torrent.info_hash == ^info_hash,
      where: piece.verified,
      where: not piece.announced,
      select: %{
        index: piece.index,
        id: piece.id,
        info_hash: torrent.info_hash
      }

    Repo.all(verified_piece_blocks_query)
  end

  def verified(count) when is_integer(count) do
    Repo.transaction(fn ->
      Repo.stream(
        from torrent in Effusion.BTP.Torrent,
        select: torrent.info_hash
      )
      |> Stream.flat_map(&verified/1)
      |> Stream.take(count)
      |> Enum.to_list()
    end)
  end

  def written(info_hash) when is_hash(info_hash) do
    written_piece_blocks_query =
      from block in Block,
      join: piece in assoc(block, :piece),
      join: torrent in assoc(piece, :torrent),
      where: torrent.info_hash == ^info_hash,
      where: piece.written,
      select: piece.index

    Repo.all(written_piece_blocks_query) |> IntSet.new()
  end

  @doc """
  Returns `true` if all pieces of this torrent have been written to disk.
  """
  def all_written?(info_hash) when is_hash(info_hash) do
    written(info_hash) |> Enum.count() == piece_count(info_hash)
  end

  @doc """
  Check if the torrent has cached or written all of the pieces it needs to be complete.
  """
  def all_present?(info_hash) when is_hash(info_hash) do
    in_memory_pieces = verified(info_hash) |> Enum.count()
    on_disk_pieces = written(info_hash) |> Enum.count()

    piece_count = piece_count(info_hash)

    in_memory_pieces + on_disk_pieces == piece_count
  end

  def piece_count(info_hash) when is_hash(info_hash) do
    Repo.one(
      from piece in Piece,
      join: torrent in assoc(piece, :torrent),
      where: torrent.info_hash == ^info_hash,
      select: count(piece.index)
    )
  end

  def torrent_length(info_hash) when is_hash(info_hash) do
    Repo.one(
      from piece in Piece,
      join: torrent in assoc(piece, :torrent),
      where: torrent.info_hash == ^info_hash,
      select: sum(piece.size)
    )
  end

  @doc """
  Get the number of bytes that have been added to this torrent.

  This includes bytes in blocks that have not yet been verified.
  """
  def bytes_completed(info_hash) when is_hash(info_hash) do
    unfinished_bytes(info_hash) +
      verified_bytes(info_hash) +
      bytes_written(info_hash)
  end

  defp unfinished_bytes(info_hash) when is_hash(info_hash) do
    info_hash
    |> unfinished()
    |> Enum.map(&Map.get(&1, :data))
    |> Enum.map(&byte_size/1)
    |> Enum.sum()
  end

  defp verified_bytes(info_hash) when is_hash(info_hash) do
    Repo.one(
      from piece in Piece,
      join: torrent in assoc(piece, :torrent),
      where: torrent.info_hash == ^info_hash,
      where: piece.verified,
      select: coalesce(sum(piece.size), 0)
    )
  end

  defp bytes_written(info_hash) when is_hash(info_hash) do
    Repo.one(
      from piece in Piece,
      join: torrent in assoc(piece, :torrent),
      where: torrent.info_hash == ^info_hash,
      where: piece.written,
      select: coalesce(sum(piece.size), 0)
    )
  end

  @doc """
  Get the number of bytes still necessary for this download to be finished.
  """
  def bytes_left(info_hash) when is_hash(info_hash) do
    if all_present?(info_hash) do
      0
    else
      torrent_length(info_hash) - bytes_completed(info_hash)
    end
  end
end
