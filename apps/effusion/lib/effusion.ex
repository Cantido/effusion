defmodule Effusion do
  @moduledoc """
  A BitTorrent library.
  """

  alias Effusion.Torrents

  def start_download(meta) do
    Torrents.start_child(meta: meta)
  end

  def stop_download(info_hash) do
    Torrents.stop(info_hash)
  end

  def progress(info_hash) do
    Torrents.get_progress(info_hash)
  end
end
