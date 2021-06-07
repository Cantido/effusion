defmodule Effusion do
  alias Effusion.Downloads

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
    Downloads.add(meta)
    |> Downloads.start()
  end

  def stop_download(info_hash) when is_hash(info_hash) do
    Effusion.Downloads.stop(info_hash)
  end
end
