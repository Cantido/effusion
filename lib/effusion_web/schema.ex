defmodule EffusionWeb.Schema do
  use Absinthe.Schema
  alias EffusionWeb.Resolvers

  import_types Absinthe.Type.Custom

  @desc "A torrent"
  object :torrent do
    field :id, :string
    field :name, :string
    field :downloaded, :integer
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
end
