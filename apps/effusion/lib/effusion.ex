defmodule Effusion do
  @moduledoc """
  A BitTorrent library.
  """

  alias Effusion.ActiveTorrent

  def start_download(meta) do
    ActiveTorrent.start_link(meta: meta)
  end

  def stop_download(info_hash) do
    ActiveTorrent.stop(info_hash)
  end

  def progress(info_hash) do
    ActiveTorrent.get_progress(info_hash)
  end
end
