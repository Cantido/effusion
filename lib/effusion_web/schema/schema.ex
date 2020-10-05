defmodule EffusionWeb.Schema do
  # use Absinthe.Schema
  # alias EffusionWeb.Resolvers
  #
  # @moduledoc """
  # Absinthe schema.
  # """
  #
  # import_types Absinthe.Type.Custom
  # import_types Effusion.Schema.Binary
  #
  # enum :torrent_status do
  #   description "The current downloading state of a torrent."
  #   value :paused, as: "paused", description: "The torrent has been halted."
  #   value :downloading, as: "downloading", description: "The torrent is actively looking for connections and downloading data."
  #   value :finished, as: "finished", description: "The torrent has downloaded and written all pieces."
  # end
  #
  # object :torrent do
  #   description """
  #   A torrent that Effusion is currently downloading.
  #   """
  #
  #   field :id, :string do
  #     description "The torrent's info hash."
  #     resolve fn torrent, _, _ ->
  #       {:ok, torrent.info_hash |> Effusion.Hash.encode()}
  #     end
  #   end
  #   field :name, :string, description: "The name of the torrent."
  #   field :downloaded, :integer do
  #     description "How many of this torrent's bytes have been downloaded."
  #     resolve &Resolvers.Torrents.downloaded/3
  #   end
  #   field :left, :integer, description: "How many of this torrent's bytes still need to be downloaded before the download is complete."
  #   field :started, :datetime, description: "When Effusion started downloading this torrent"
  #   field :state, :torrent_status, description: "The current downloading state of the torrent."
  #   field :announce, :string, description: "The main announce URL to send download updates to."
  #   field :size, :integer, description: "The total number of bytes in this torrent."
  #   field :piece_size, :integer, description: "The nominal number of bytes in each piece of this torrent."
  #   field :comment, :string, description: "The comment left in the torrent's metadata file."
  #   field :created_by, :string, description: "The person or program that assembled this torrent's metadata file."
  #   field :creation_date, :datetime, description: "The date the torrent file was created."
  #   field :last_announce, :datetime, description: "The last time the announce server was contacted."
  #   field :next_announce, :datetime, description: "The next time the announce server should be contacted."
  #   field :connected_peers_count, :integer do
  #     description "The number of peers we are currently connected to."
  #     resolve &Resolvers.Torrents.connected_peers_count/3
  #   end
  #   field :available_peers_count, :integer do
  #     description "The number of peers for this torrent that the application can contact."
  #     resolve &Resolvers.Torrents.available_peers_count/3
  #   end
  # end
  #
  # object :session do
  #   description "Global statistics across all torrents"
  #   field :download_bytes_per_second, :integer do
  #     description "The average number of downloaded bytes per second."
  #     resolve &Resolvers.Sessions.download_bytes_per_second/3
  #   end
  #   field :upload_bytes_per_second, :integer do
  #     description "The average number of uploaded bytes per second."
  #     resolve &Resolvers.Sessions.upload_bytes_per_second/3
  #   end
  # end
  #
  # query do
  #   field :session, :session do
  #     description "Get global statistics across all torrents."
  #     # all stats will come from field resolvers
  #     resolve fn _, _, _ -> {:ok, %{}} end
  #   end
  #
  #   field :torrents, list_of(:torrent) do
  #     description "Get all torrents that are being downloaded."
  #     resolve &Resolvers.Torrents.all_torrents/3
  #   end
  #
  #   field :torrent, :torrent do
  #     description "Get a torrent"
  #     arg :id, non_null(:id), description: "The info hash of the torrent to get."
  #     resolve &Resolvers.Torrents.find_torrent/3
  #   end
  # end
  #
  # mutation do
  #   field :add_torrent, type: :torrent do
  #     description "Add a torrent metainfo file to start downloading"
  #     arg :meta, non_null(:binary), description: "The base64-encoded metadata file (.torrent file) of the torrent to download."
  #     resolve &Resolvers.Torrents.add_torrent/3
  #   end
  #
  #   field :pause_torrent, type: :torrent do
  #     description "Halt a torrent's downloading."
  #     arg :id, non_null(:id), description: "The info hash of the torrent to pause."
  #     resolve &Resolvers.Torrents.pause_torrent/3
  #   end
  #
  #   field :start_torrent, type: :torrent do
  #     description "Start or restart torrent's downloading."
  #     arg :id, non_null(:id), description: "The info hash of the torrent to start."
  #     resolve &Resolvers.Torrents.start_torrent/3
  #   end
  # end
end
