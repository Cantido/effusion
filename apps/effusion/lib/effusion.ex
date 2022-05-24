defmodule Effusion do
  @moduledoc """
  A BitTorrent library.
  """

  alias Effusion.Torrents
  alias Effusion.Swarm

  def downloading?(info_hash) do
    Torrents.downloading?(info_hash)
  end

  def start_download(meta) do
    Torrents.start_child(meta: meta)
  end

  def pause_download(info_hash) do
    Torrents.pause(info_hash)
  end

  def stop_download(info_hash) do
    Torrents.stop(info_hash)
  end

  def progress(info_hash) do
    Torrents.get_progress(info_hash)
  end

  def peers(info_hash) do
    Swarm.peers_for_torrent(info_hash)
  end

  def add_peer(info_hash, address) do
    Swarm.add_peer(address)
    Swarm.add_torrent(address, info_hash)
  end
end
