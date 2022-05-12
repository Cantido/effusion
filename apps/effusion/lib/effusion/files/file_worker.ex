defmodule Effusion.FileWorker do
  alias Effusion.Torrents

  def write_block(data, index, offset, meta) do
    with :ok <- Effusion.IO.write_block(data, index, offset, meta.info) do
      Torrents.block_written(meta.info_hash, index, offset)
    end
  end

  def read_block(index, offset, size, meta) do
    {:ok, data} = Effusion.IO.read_block(index, offset, size, meta.info)
    data
  end
end
