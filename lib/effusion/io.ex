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

  defp write_piece_multi_file(info, destdir, %{index: i, data: d}) when is_binary(d) do
    path = Path.join(destdir, info.name)
    :ok = File.mkdir_p(path)

    file_bytes = split_bytes_to_files(info, %{index: i, data: d})

    _ = Enum.map(file_bytes, fn {rel_path, {pos, data}} ->
      file = Path.join(path, rel_path)
      {:ok, device} = File.open(file, [:read, :write])
      :ok = :file.pwrite(device, {:bof, pos}, [data])
    end)
    :ok
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

      {Path.join(f.path), {file_offset, file_data}}
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

  defp open(info, destdir) do
    with path <- Path.join(destdir, info.name),
         :ok <- File.mkdir_p(destdir),
         do: File.open(path, [:read, :write])
  end

  defp write_piece_single_file(info, destdir, piece = %{data: d}) when is_binary(d) do
    with {:ok, device} <- open(info, destdir),
    do: do_write(info, device, piece)
  end

  defp do_write(info, device, %{index: i, data: d}) do
    with piece_start_byte <- i * info.piece_length,
         :ok <- :file.pwrite(device, {:bof, piece_start_byte}, [d])
    do
      _ = File.close(device)
      :ok
    else
      err ->
        _ = File.close(device)
        err
    end
  end
end
