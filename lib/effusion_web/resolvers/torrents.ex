defmodule EffusionWeb.Resolvers.Torrents do
  alias Effusion.BTP.TorrentRegistry
  alias Effusion.BTP.DownloadServer
  def all_torrents(_root, _args, _info) do
    info_hashes = TorrentRegistry.all_torrents

    torrents = info_hashes
      |> Enum.map(&DownloadServer.get/1)
      |> Enum.map(&build/1)

    {:ok, torrents}
  end

  def find_torrent(_parent, %{id: id}, _resolution) do
    if TorrentRegistry.present?(id) do
      {:ok, DownloadServer.get(id) |> build()}
    else
      {:error, "Torrent ID #{Effusion.Hash.inspect id} not found"}
    end
  end

  defp build(download) do
    %{
      id: download.meta.info_hash,
      name: download.meta.info.name
    }
  end
end
