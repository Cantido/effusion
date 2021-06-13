defmodule Effusion.FileWorker do
  alias Effusion.ActiveDownload

  def write_piece(data, index, meta) do
    with :ok <- Effusion.IO.write_piece(data, index, meta.info) do
      ActiveDownload.piece_written(meta.info_hash, index)
    end
  end
end
