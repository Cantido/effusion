defmodule Effusion do
  @moduledoc """
  A BitTorrent library.
  """

  alias Effusion.DownloadManager

  def start_download(meta) do
    DownloadManager.start_link(meta: meta)
  end
end
