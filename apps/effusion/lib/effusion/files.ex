defmodule Effusion.Files do
  alias Effusion.Torrents

  def write_block(info_hash, index, offset, data) do
    unless Torrents.block_written?(info_hash, index, offset) do
      meta = Torrents.get_meta(info_hash)

      :ok = Effusion.Files.IO.write_block(
        data,
        index,
        offset,
        meta.info
      )

      Solvent.publish(
        "io.github.cantido.effusion.blocks.written",
        subject: info_hash,
        data: %{
          index: index,
          offset: offset,
          size: byte_size(data)
        }
      )
    end
  end
end
