defmodule EffusionWeb.Schema do
  use Absinthe.Schema
  alias EffusionWeb.Resolvers

  import_types Absinthe.Type.Custom
  import_types Effusion.Schema.Binary

  @desc "A torrent"
  object :torrent do
    field :id, :string do
      resolve fn torrent, _, _ ->
        {:ok, torrent.info_hash |> Effusion.Hash.encode()}
      end
    end
    field :name, :string
    field :downloaded, :integer do
      resolve &Resolvers.Torrents.downloaded/3
    end
    field :left, :integer
    field :started_at, :datetime
  end

  query do
    @desc "Get all torrents"
    field :torrents, list_of(:torrent) do
      resolve &Resolvers.Torrents.all_torrents/3
    end

    @desc "Get a torrent"
    field :torrent, :torrent do
      arg :id, non_null(:id)
      resolve &Resolvers.Torrents.find_torrent/3
    end
  end

  mutation do
    @desc "Add a torrent metainfo file to start downloading"
    field :add_torrent, type: :torrent do
      arg :meta, non_null(:binary)

      resolve &Resolvers.Torrents.add_torrent/3
    end
  end
end