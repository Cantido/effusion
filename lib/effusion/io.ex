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

  def write_piece(torrent, destdir, %{index: i, data: d}) when is_binary(d) do
    path = Path.join(destdir, torrent.info.name)
    :ok = File.mkdir_p(destdir)
    {:ok, file} = File.open(path, [:read, :write])

    with piece_start_byte <- i * torrent.info.piece_length,
         _ = Logger.debug("writing #{inspect(d)} to #{inspect(file)}"),
         :ok <- :file.pwrite(file, {:bof, piece_start_byte}, [d]),
         torrent <- torrent
         |> Map.update(:written, IntSet.new(i), &IntSet.put(&1, i))
    do
      :ok = File.close(file)
      {:ok, torrent}
    else
      err ->
        _ = File.close(file)
        err
    end
  end
end
