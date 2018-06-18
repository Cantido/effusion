defmodule Effusion.IO do
  require Logger
  alias Effusion.BTP.Torrent

  def write_to(torrent, file) do
    write_pieces(torrent, file, Enum.to_list(Torrent.pieces(torrent)))
  end

  defp write_pieces(torrent, file, [piece | rest]) do
    {:ok, torrent} = write_piece(torrent, file, piece)

    write_pieces(torrent, file, rest)
  end

  defp write_pieces(torrent, _file, []) do
    {:ok, torrent}
  end

  def write_piece(torrent, file, %{index: i, data: d}) when is_binary(d) do
    piece_start_byte = i * torrent.info.piece_length
    _ = Logger.debug("writing #{inspect(d)} to #{inspect(file)}")
    :ok = :file.pwrite(file, {:bof, piece_start_byte}, [d])
    torrent = torrent
      |> Map.update(:written, IntSet.new(i), &IntSet.put(&1, i))
    {:ok, torrent}
  end
end
