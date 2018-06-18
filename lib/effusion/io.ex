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

  defp open(torrent, destdir) do
    with path <- Path.join(destdir, torrent.info.name),
         :ok <- File.mkdir_p(destdir),
         do: File.open(path, [:read, :write])
  end

  def write_piece(torrent, destdir, %{index: i, data: d}) when is_binary(d) do
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
