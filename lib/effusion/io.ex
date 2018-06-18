defmodule Effusion.IO do
  require Logger
  alias Effusion.BTP.Torrent

  def write_to(torrent, file) do
    write_pieces(torrent, file, Enum.to_list(Torrent.pieces(torrent)))
  end

  defp write_pieces(torrent, file, [piece | rest]) do
    with {:ok, torrent} <- write_piece(torrent, file, piece),
         do: write_pieces(torrent, file, rest)
  end

  defp write_pieces(torrent, _file, []) do
    {:ok, torrent}
  end

  def write_piece(torrent, destdir, block) do
    if Map.has_key?(torrent.info, :files) do
      write_piece_multi_file(torrent, destdir, block)
    else
      write_piece_single_file(torrent, destdir, block)
    end
  end

  defp write_piece_multi_file(torrent, destdir, %{index: i, data: d}) when is_binary(d) do
    path = Path.join(destdir, torrent.info.name)
    :ok = File.mkdir_p(path)

    # case 1: piece is entirely within a single dest file
    # case 2: piece overlaps multiple files (as with hello_world.torrent)

    # paths, with position & bytes to write.

    file_bytes = split_bytes_to_files(torrent, %{index: i, data: d})

    Enum.map(file_bytes, fn {rel_path, {pos, data}} ->
      file = Path.join(path, rel_path)
      {:ok, device} = File.open(file, [:read, :write])
      :ok = :file.pwrite(device, {:bof, pos}, [data])
    end)

    Map.update(torrent, :written, IntSet.new(i), &IntSet.put(&1, i))
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
         :ok <- :file.pwrite(device, {:bof, piece_start_byte}, [d]),
         torrent <- Map.update(torrent, :written, IntSet.new(i), &IntSet.put(&1, i))
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
