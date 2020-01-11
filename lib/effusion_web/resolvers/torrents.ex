defmodule EffusionWeb.Resolvers.Torrents do
  alias Effusion.BTP.Metainfo
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Torrent
  alias Effusion.Repo

  def all_torrents(_root, _args, _info) do
    torrents = Repo.all(Torrent)

    {:ok, torrents}
  end

  def find_torrent(_parent, %{id: id}, _resolution) do
    binary_id = Effusion.Hash.decode(id)
    Torrent.by_info_hash(binary_id)
  end

  def downloaded(torrent, _args, _info) do
    {:ok, Pieces.bytes_completed(torrent.info_hash)}
  end

  def add_torrent(_root, %{meta: meta}, _info) do
    {:ok, metainfo} = Metainfo.decode(meta)
    {:ok, info_hash} = Effusion.start_download(metainfo)

    Torrent.by_info_hash(info_hash)
  end

  def pause_torrent(_root, %{id: id}, _info) do
    binary_id = Effusion.Hash.decode(id)
    Effusion.pause_download(binary_id)
    Torrent.by_info_hash(binary_id)
  end

  def start_torrent(_root, %{id: id}, _info) do
    binary_id = Effusion.Hash.decode(id)
    Effusion.start_download(binary_id)
    Torrent.by_info_hash(binary_id)
  end
end
