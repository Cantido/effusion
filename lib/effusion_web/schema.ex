defmodule EffusionWeb.Schema do
  use Absinthe.Schema
  alias EffusionWeb.Resolvers.Torrents

  object :torrent do
    field :id, :string
    field :name, :string
  end

  query do
    field :torrents, list_of(:torrent) do
      resolve &Torrents.all_torrents/3
    end

    @desc "Get a torrent"
    field :torrent, :torrent do
      arg :id, non_null(:id)
      resolve &Torrents.find_torrent/3
    end
  end
end
