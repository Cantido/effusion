defmodule EffusionWeb.Schema do
  use Absinthe.Schema
  alias EffusionWeb.Resolvers.Torrents

  object :torrent do
    field :name, :string
  end

  query do
    field :torrents, list_of(:torrent) do
      resolve &Torrents.all_torrents/3
    end
  end
end
