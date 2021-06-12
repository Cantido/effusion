defmodule Effusion do
  @moduledoc """
  A BitTorrent library.
  """

  alias Effusion.ActiveDownload

  def start_download(meta) do
    ActiveDownload.start_link(meta: meta)
  end

  def stop_download(info_hash) do
    ActiveDownload.stop(info_hash)
  end
end
