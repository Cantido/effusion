defmodule Effusion.IO do
  require Logger
  alias Effusion.BTP.Block
  alias Effusion.BTP.File, as: BTPFile
  alias Effusion.BTP.Piece
  alias Effusion.BTP.Torrent
  alias Effusion.Repo

  @moduledoc """
  Functions for reading and writing files described by torrents.
  """

  @doc """
  Pull the piece with the given index out of the database and write it out to the configured file.
  """
  def write_piece(piece = %Piece{}) do
    piece = Repo.preload(piece, [:torrent])
    info_hash = piece.torrent.info_hash
    index = piece.index

    :telemetry.execute(
      [:io, :write, :piece, :starting],
      %{},
      %{info_hash: info_hash, index: index})

    piece_data = Block.aggregate_data(info_hash, index) |> Repo.one!()
    Logger.debug("Data for piece #{index}: #{inspect piece_data}")
    torrent = Torrent.get(info_hash) |> Repo.one!()
    files = BTPFile.get(info_hash) |> Repo.all()

    {duration, results} = :timer.tc(fn ->
      write_files(files, torrent.name, torrent.piece_size, index, piece_data)
    end)

    :telemetry.execute(
      [:io, :write, :piece, :success],
      %{duration: duration},
      %{info_hash: info_hash, index: index})

    results
  end

  defp write_files(files, name, piece_length, index, data) do
    destdir = Application.get_env(:effusion, :download_destination)

    BTPFile.split_bytes_to_files(destdir, files, name, piece_length, index, data)
    |> Enum.group_by(fn {path, _} -> path end, fn {_, locbytes} -> locbytes end)
    |> Enum.map(fn {path, locbytes} -> {path, write_file(path, locbytes)} end)
  end

  defp write_file(path, locbytes) when is_list(locbytes) do
    with :ok <- File.mkdir_p(Path.dirname(path)),
         {:ok, device} <- File.open(path, [:read, :write]),
         :ok <- :file.pwrite(device, locbytes) do
      File.close(device)
    end
  end
end
