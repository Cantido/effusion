defmodule Effusion do
  @moduledoc """
  A BitTorrent library.
  """

  alias Effusion.DownloadManager

  def start_download(meta) do
    DownloadManager.start_link(meta: meta)
  end

  def stop_download(info_hash) do
    DownloadManager.stop(info_hash)
  end
end
