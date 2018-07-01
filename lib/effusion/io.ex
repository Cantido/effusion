defmodule Effusion.IO do
  require Logger
  alias Effusion.BTP.Torrent

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
    write_pieces(torrent, directory, Enum.to_list(Torrent.verified(torrent)))
  end

  def write_pieces(torrent = %{info: info}, destdir, pieces) do
    do_write_pieces(info, destdir, pieces)

    torrent = Enum.reduce(pieces, torrent, fn p, t -> Torrent.mark_piece_written(t, p) end)
    {:ok, torrent}
  end

  def write_piece(info, destdir, block) do
    do_write_pieces(info, destdir, [block])
  end

  defp do_write_pieces(info, destdir, pieces) do
    pieces
    |> Enum.flat_map(fn %{index: i, data: d} -> split_bytes_to_files(destdir, info, %{index: i, data: d}) end)
    |> Enum.group_by(fn {path, _} -> path end, fn {_, locbytes} -> locbytes end)
    |> Enum.each(fn {path, locbytes} -> write_chunk(path, locbytes) end)
  end

  defp write_chunk(path, locbytes) when is_list(locbytes) do
    with :ok <- File.mkdir_p(Path.dirname(path)),
         {:ok, device} <- File.open(path, [:read, :write])
    do
      write_result = :file.pwrite(device, locbytes)
      _ = File.close(device)
      write_result
    end
  end

  defp split_bytes_to_files(destdir, info = %{files: _}, %{index: i, data: d}) do
    piece_start = i * info.piece_length
    piece_end = piece_start + byte_size(d)

    info.files
    |> Enum.with_index()
    |> Enum.filter(fn {f, fi} ->
      file_start = first_byte_index(info, fi)
      file_end = file_start + f.length

      file_start <= piece_end && piece_start <= file_end
    end)
    |> Enum.map(fn {f, fi} ->
      file_start = first_byte_index(info, fi)
      file_end = file_start + f.length

      chunk_start = max(piece_start, file_start)
      chunk_end = min(file_end, piece_end)

      binary_part_pos = max(0, file_start - piece_start)
      binary_part_len = chunk_end - chunk_start

      file_offset = max(0, piece_start - file_start)

      file_data = :binary.part(d, binary_part_pos, binary_part_len)

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
    |> Enum.map(&(&1.length))
    |> Enum.sum()
  end
end
