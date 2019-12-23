defmodule Effusion.IO do
  require Logger
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.Piece
  alias Effusion.BTP.Block
  alias Effusion.Range
  alias Effusion.Repo

  @moduledoc """
  Functions for reading and writing files described by torrents.
  """

  @doc """
  Pull the piece with the given index out of the database and write it out to the configured file.
  """
  def write_piece(info_hash, %{index: index}) when is_integer(index) and index >= 0 do
    :telemetry.execute(
      [:io, :write, :piece, :starting],
      %{},
      %{info_hash: info_hash, index: index})

    piece_data = Block.aggregate_data(info_hash, index) |> Repo.one!()
    torrent = Torrent.get(info_hash) |> Repo.one!()
    files = Effusion.BTP.File.get(info_hash) |> Repo.all()

    {latency, _} = :timer.tc(fn ->
      :ok = do_write_pieces(files, torrent.name, torrent.piece_size, [%{index: index, data: piece_data}])
    end)

    :telemetry.execute(
      [:io, :write, :piece, :success],
      %{latency: latency},
      %{info_hash: info_hash, index: index})
  end

  defp do_write_pieces(files, name, piece_length, pieces) do
    destdir = Application.get_env(:effusion, :download_destination)
    pieces
    |> Enum.flat_map(fn %{index: i, data: d} ->
      split_bytes_to_files(destdir, files, name, piece_length, %{index: i, data: d})
    end)
    |> Enum.group_by(fn {path, _} -> path end, fn {_, locbytes} -> locbytes end)
    |> Enum.each(fn {path, locbytes} -> write_chunk(path, locbytes) end)

    :ok
  end

  defp write_chunk(path, locbytes) when is_list(locbytes) do
    with :ok <- File.mkdir_p(Path.dirname(path)),
         {:ok, device} <- File.open(path, [:read, :write]) do
      write_result = :file.pwrite(device, locbytes)
      _ = File.close(device)
      write_result
    end

    :ok
  end

  defp split_bytes_to_files(destdir, [], name, piece_length, %{index: i, data: d}) do
    Map.new([{Path.join(destdir, name), {i * piece_length, d}}])
  end

  defp split_bytes_to_files(destdir, files, name, piece_length, %{index: i, data: d}) do
    piece_range = Range.poslen(i * piece_length, byte_size(d))

    files
    |> Enum.with_index()
    |> Enum.map(fn {f, fi} ->
      file_start = first_byte_index(files, fi)
      file_range = Range.poslen(file_start, f.size)
      {f, file_range}
    end)
    |> Enum.filter(fn {_f, file_range} ->
      Range.overlap?(file_range, piece_range)
    end)
    |> Enum.map(fn {f, file_range = file_start.._} ->
      overlap = Range.overlap(file_range, piece_range)
      poslen = Range.overlap_poslen(file_range, piece_range)

      file_offset.._ = Effusion.Range.shift(overlap, -file_start)
      file_data = :binary.part(d, poslen)

      {Path.join([destdir, name, f.path]), {file_offset, file_data}}
    end)
  end

  defp first_byte_index(_torrent, file_index) when file_index == 0 do
    0
  end

  defp first_byte_index(files, file_index) when file_index > 0 do
    files
    |> Enum.slice(0..(file_index - 1))
    |> Enum.map(& &1.size)
    |> Enum.sum()
  end
end
