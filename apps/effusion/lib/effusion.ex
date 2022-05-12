defmodule Effusion do
  @moduledoc """
  A BitTorrent library.
  """

  alias Effusion.Torrents
  alias Effusion.Swarm

  def start_download(meta) do
    Torrents.start_child(meta: meta)
  end

  def stop_download(info_hash) do
    Torrents.stop(info_hash)
  end

  def progress(info_hash) do
    Torrents.get_progress(info_hash)
  end

  def peers(info_hash) do
    Torrents.peers(info_hash)
  end

  def add_peer(info_hash, ip, port) do
    Swarm.add_peer(ip, port)
    Torrents.add_peer(info_hash, {ip, port})
  end
end
