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

  defp write_pieces(torrent, file, [piece | rest]) do
    with {:ok, torrent} <- write_piece(torrent, file, piece)
    do
      torrent
      |> Torrent.mark_piece_written(piece)
      |> write_pieces(file, rest)
   end
  end

  defp write_pieces(torrent, _file, []) do
    {:ok, torrent}
  end

  defp write_piece(torrent, destdir, block) do
    if Map.has_key?(torrent.info, :files) do
      write_piece_multi_file(torrent, destdir, block)
    else
      write_piece_single_file(torrent, destdir, block)
    end
  end

  defp write_piece_multi_file(torrent, destdir, %{index: i, data: d}) when is_binary(d) do
    path = Path.join(destdir, torrent.info.name)
    :ok = File.mkdir_p(path)

    file_bytes = split_bytes_to_files(torrent, %{index: i, data: d})

    _ = Enum.map(file_bytes, fn {rel_path, {pos, data}} ->
      file = Path.join(path, rel_path)
      {:ok, device} = File.open(file, [:read, :write])
      :ok = :file.pwrite(device, {:bof, pos}, [data])
    end)
    torrent
  end


  # Splits a piece into a map of paths and the data & offset to write
  # to that path.
  #
  # Example
  #
  #    %{
  #      "hello.txt" => {0, "Hello\n"},
  #      "world.txt" => {0, "world!\n"}
  #     }
  #
  defp split_bytes_to_files(torrent, %{index: i, data: d}) do
    piece_start = i * torrent.info.piece_length
    piece_end = piece_start + byte_size(d)

    torrent.info.files
    |> Enum.with_index()
    |> Enum.filter(fn {f, fi} ->
      file_start = first_byte_index(torrent, fi)
      file_end = file_start + f.length

      file_start <= piece_end && piece_start <= file_end
    end)
    |> Enum.map(fn {f, fi} ->
      file_start = first_byte_index(torrent, fi)
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

  defp first_byte_index(torrent, file_index) when file_index > 0 do
    torrent.info.files
    |> Enum.slice(0..(file_index - 1))
    |> Enum.map(&(&1.length))
    |> Enum.sum()
  end

  defp open(torrent, destdir) do
    with path <- Path.join(destdir, torrent.info.name),
         :ok <- File.mkdir_p(destdir),
         do: File.open(path, [:read, :write])
  end

  defp write_piece_single_file(torrent, destdir, %{index: i, data: d}) when is_binary(d) do
    {:ok, device} = open(torrent, destdir)

    with piece_start_byte <- i * torrent.info.piece_length,
         :ok <- :file.pwrite(device, {:bof, piece_start_byte}, [d])
    do
      _ = File.close(device)
      {:ok, torrent}
    else
      err ->
        _ = File.close(device)
        err
    end
  end
end
