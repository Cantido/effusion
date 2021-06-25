defmodule Effusion.FileWorker do
  alias Effusion.ActiveTorrent

  def write_piece(data, index, meta) do
    with :ok <- Effusion.IO.write_piece(data, index, meta.info) do
      ActiveTorrent.piece_written(meta.info_hash, index)
    end
  end
end
