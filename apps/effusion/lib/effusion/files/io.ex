defmodule Effusion.IO do
  alias Effusion.Range
  alias Effusion.Metadata
  require Logger

  def write_block(data, index, offset, info) do
    write_chunk(data, (index * info.piece_length) + offset, info)
  end

  def write_piece(data, index, info) do
    write_chunk(data, index * info.piece_length, info)
  end

  defp write_chunk(data, byte_offset, info) do
    destdir = Application.fetch_env!(:effusion, :download_destination)

    split_bytes_to_files(info, byte_offset, data)
    |> Enum.group_by(fn {path, _} -> path end, fn {_, locbytes} -> locbytes end)
    |> Enum.each(fn {rel_path, locbytes} ->
      path = Path.join(destdir, rel_path)

      with :ok <- File.mkdir_p(Path.dirname(path)),
          {:ok, device} <- File.open(path, [:read, :write]),
          :ok <- :file.pwrite(device, locbytes) do
        File.close(device)
      end
    end)

    :ok
  end

  def split_bytes_to_files(info, byte_offset, data) do
    piece_range = Range.from_poslen(byte_offset, byte_size(data))

    Metadata.file_byte_ranges(info)
    |> Enum.filter(fn {_path, file_range} ->
      Range.overlap?(file_range, piece_range)
    end)
    |> Enum.map(fn {path, file_range = file_start.._} ->
      poslen = Range.overlap_poslen(file_range, piece_range)
      file_data = :binary.part(data, poslen)

      overlap = Range.overlap(file_range, piece_range)
      file_offset.._ = Effusion.Range.shift(overlap, -file_start)

      {path, {file_offset, file_data}}
    end)
  end

  def read_block(index, offset, size, info) do
    block_range = Range.from_poslen(index * info.piece_length + offset, size)
    destdir = Application.fetch_env!(:effusion, :download_destination)
    file_ranges =
      Metadata.file_byte_ranges(info)
      |> Enum.filter(fn {_f, file_range} ->
        Range.overlap?(file_range, block_range)
      end)
      |> Enum.map(fn {path, file_range = file_start.._} ->
        overlap = Range.overlap(file_range, block_range)
        file_range = Range.shift(overlap, -file_start)

        {path, Range.to_poslen(file_range)}
      end)

    block_chunks =
      file_ranges
      |> Enum.map(fn {rel_path, {start, length}} ->
        path = Path.join(destdir, rel_path)

        with :ok <- File.mkdir_p(Path.dirname(path)),
            {:ok, device} <- File.open(path, [:read]),
            {:ok, bytes} <- :file.pread(device, start, length) do
          File.close(device)
          bytes
        end
      end)

    errors = Enum.flat_map(
      block_chunks,
      fn
        {:error, err} -> [err]
        _ -> []
      end
    )

    if Enum.empty?(errors) do
      {:ok, Enum.join(block_chunks)}
    else
      {:error, errors}
    end
  end
end
