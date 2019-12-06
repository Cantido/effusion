defmodule EffusionWeb.Resolvers.Torrents do
  alias Effusion.BTP.Metainfo
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Torrent
  alias Effusion.Repo
  import Ecto.Query

  def all_torrents(_root, _args, _info) do
    torrents = Repo.all(Torrent)

    {:ok, torrents}
  end

  def find_torrent(_parent, %{id: id}, _resolution) do
    binary_id = Effusion.Hash.decode(id)
    torrent_query = from torrent in Torrent,
                      where: torrent.info_hash == ^binary_id

    torrent = Repo.one(torrent_query)
    if torrent != nil do
      {:ok, torrent}
    else
      {:error, "Torrent ID #{id} not found"}
    end
  end

  def downloaded(torrent, _args, _info) do
    {:ok, Pieces.bytes_completed(torrent.info_hash)}
  end

  def add_torrent(_root, %{meta: meta}, _info) do
    {:ok, metainfo} = Metainfo.decode(meta)
    {:ok, info_hash} = Effusion.start_download(metainfo)

    torrent_query = from torrent in Torrent,
                      where: torrent.info_hash == ^info_hash

    torrent = Repo.one(torrent_query)
    if torrent != nil do
      {:ok, torrent}
    else
      {:error, "Torrent ID #{Effusion.Hash.encode info_hash} not found"}
    end
  end
end
