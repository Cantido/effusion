defmodule Effusion do
  import Effusion.Hash, only: [is_hash: 1]

  @moduledoc """
  A BitTorrent library.
  """

  @doc """
  Start asynchronously downloading the torrent described by `meta`,
  and write it to a file in the `destfile` directory.

  This function returns immediately.
  """
  def start_download(meta) when is_map(meta) do
    Effusion.Downloads.add(meta)
    Effusion.Downloads.start(meta.info_hash)
  end

  def stop_download(info_hash) when is_hash(info_hash) do
    Effusion.Downloads.stop(info_hash)
  end
end
