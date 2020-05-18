defmodule EffusionWeb.Resolvers.Torrents do
  alias Effusion.BTP.Peer
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Torrent
  alias Effusion.Repo

  def all_torrents(_root, _args, _info) do
    torrents = Torrent.all() |> Repo.all()

    {:ok, torrents}
  end

  def find_torrent(_parent, %{id: id}, _resolution) do
    binary_id = Effusion.Hash.decode(id)
    Torrent.by_info_hash(binary_id)
  end

  def downloaded(torrent, _args, _info) do
    {:ok, Pieces.bytes_completed(torrent.info_hash)}
  end

  def connected_peers_count(torrent, _args, _info) do
    info_hash = torrent.info_hash

    connected_peers_count = info_hash
    |> Peer.connected_query()
    |> Repo.aggregate(:count, :id)

    {:ok, connected_peers_count}
  end

  def available_peers_count(torrent, _args, _info) do
    info_hash = torrent.info_hash

    available_peers_count = info_hash
    |> Peer.all()
    |> Repo.aggregate(:count, :id)

    {:ok, available_peers_count}
  end

  def add_torrent(_root, %{meta: meta}, _info) do
    {:ok, metainfo} = Metatorrent.decode(meta)
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
