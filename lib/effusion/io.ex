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

  def write_pieces(torrent = %{info: info}, file, [piece | rest]) do
    with :ok <- write_piece(info, file, piece),
         torrent = Torrent.mark_piece_written(torrent, piece),
         {:ok, torrent} <- write_pieces(torrent, file, rest)
    do
      {:ok, torrent}
   end
  end

  def write_pieces(torrent, _file, []) do
    {:ok, torrent}
  end

  def write_piece(info, destdir, block) do
    if Map.has_key?(info, :files) do
      write_piece_multi_file(info, destdir, block)
    else
      write_piece_single_file(info, destdir, block)
    end
  end

  defp write_piece_single_file(info, destdir, %{index: i, data: d}) when is_binary(d) do
    piece_start_byte = i * info.piece_length
    write_chunk(destdir, info.name, piece_start_byte, d)
  end

  defp write_piece_multi_file(info, destdir, %{index: i, data: d}) when is_binary(d) do
    with torrent_root_dir = Path.join(destdir, info.name),
         :ok <- File.mkdir_p(torrent_root_dir)
    do
      file_bytes = split_bytes_to_files(info, %{index: i, data: d})
      _ = Enum.map(file_bytes, fn {file_rel_path, {pos, data}} ->
        {file_rel_dir, [file_name]} = Enum.split(file_rel_path, -1)
        file_dir = Path.join(torrent_root_dir, file_rel_dir)
        write_chunk(file_dir, file_name, pos, data)
      end)
      :ok
    end
  end

  defp write_chunk(destdir, name, pos, data) do
    with path = Path.join(destdir, name),
         :ok <- File.mkdir_p(destdir),
         {:ok, device} <- File.open(path, [:read, :write])
    do
      write_result = :file.pwrite(device, {:bof, pos}, [data])
      _ = File.close(device)
      write_result
    end
  end

  defp split_bytes_to_files(info, %{index: i, data: d}) do
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

      {f.path, {file_offset, file_data}}
    end)
    |> Map.new()
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
