defmodule Effusion.IO do
  require Logger
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Block
  alias Effusion.BTP.Metainfo
  alias Effusion.Repo
  import Ecto.Query

  @moduledoc """
  Functions for reading and writing files described by torrents.
  """

  @doc """
  Writes the completed pieces of `torrent` to a file in `directory`.

  If the torrent describes a single file, then that file with be created
  in `directory` with the torrent's `:name`.
  If `torrent` describes multiple files,
  then a directory with the torrent's `:name` will be created in `directory`,
  and the torrent's files will be written to that directory.

  The `torrent` will have its in-memory pieces cleared,
  and will be updated to remember the pieces that were written.
  """
  def write_to(torrent, directory) do
    write_pieces(torrent, directory, Enum.to_list(Pieces.verified(torrent)))
  end

  def write_pieces(torrent = %{info: info}, destdir, pieces) do
    do_write_pieces(info, destdir, pieces)

    torrent = Enum.reduce(pieces, torrent, fn p, t -> Pieces.mark_piece_written(t, p) end)
    {:ok, torrent}
  end

  def write_piece(info_hash, destdir, %{index: index}) do
    Logger.debug("Writing piece #{index} for #{info_hash |> Effusion.Hash.inspect()}...")
    info = Metainfo.get_meta(info_hash).info

    data_query = from block in Block,
                  join: piece in assoc(block, :piece),
                  join: torrent in assoc(piece, :torrent),
                  where: torrent.info_hash == ^info_hash,
                  where: piece.index == ^index,
                  order_by: block.offset,
                  select: block.data
    data_blocks = Repo.all(data_query)

    piece_data = Enum.reduce(data_blocks, <<>>, fn data, bin ->
      bin <> data
    end)

    ret = do_write_pieces(info, destdir, [%{index: index, data: piece_data}])
    Logger.debug("Done writing piece #{index} for #{info_hash |> Effusion.Hash.inspect()}")
  end

  defp do_write_pieces(info, destdir, pieces) do
    pieces
    |> Enum.flat_map(fn %{index: i, data: d} ->
      split_bytes_to_files(destdir, info, %{index: i, data: d})
    end)
    |> Enum.group_by(fn {path, _} -> path end, fn {_, locbytes} -> locbytes end)
    |> Enum.each(fn {path, locbytes} -> write_chunk(path, locbytes) end)
  end

  defp write_chunk(path, locbytes) when is_list(locbytes) do
    with :ok <- File.mkdir_p(Path.dirname(path)),
         {:ok, device} <- File.open(path, [:read, :write]) do
      write_result = :file.pwrite(device, locbytes)
      _ = File.close(device)
      write_result
    end
  end

  defp split_bytes_to_files(destdir, info = %{files: _}, %{index: i, data: d}) do
    piece_range = Effusion.Range.poslen(i * info.piece_length, byte_size(d))

    info.files
    |> Enum.with_index()
    |> Enum.map(fn {f, fi} ->
      file_start = first_byte_index(info, fi)
      file_range = Effusion.Range.poslen(file_start, f.length)
      {f, file_range}
    end)
    |> Enum.filter(fn {_f, file_range} ->
      Effusion.Range.overlap?(file_range, piece_range)
    end)
    |> Enum.map(fn {f, file_range = file_start.._} ->
      overlap = Effusion.Range.overlap(file_range, piece_range)
      poslen = Effusion.Range.overlap_poslen(file_range, piece_range)

      file_offset.._ = Effusion.Range.shift(overlap, -file_start)
      file_data = :binary.part(d, poslen)

      {Path.join([destdir, info.name, f.path]), {file_offset, file_data}}
    end)
  end

  defp split_bytes_to_files(destdir, info, %{index: i, data: d}) do
    Map.new([{Path.join(destdir, info.name), {i * info.piece_length, d}}])
  end

  defp first_byte_index(_torrent, file_index) when file_index == 0 do
    0
  end

  defp first_byte_index(info, file_index) when file_index > 0 do
    info.files
    |> Enum.slice(0..(file_index - 1))
    |> Enum.map(& &1.length)
    |> Enum.sum()
  end
end
