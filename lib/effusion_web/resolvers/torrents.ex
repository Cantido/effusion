defmodule EffusionWeb.Resolvers.Torrents do
  alias Effusion.BTP.TorrentRegistry
  alias Effusion.BTP.DownloadServer
  def all_torrents(_root, _args, _info) do
    info_hashes = TorrentRegistry.all_torrents

    torrents = info_hashes
      |> Enum.map(&DownloadServer.get/1)
      |> Enum.map(&(%{name: &1.meta.info.name}))

    {:ok, torrents}
  end
end
