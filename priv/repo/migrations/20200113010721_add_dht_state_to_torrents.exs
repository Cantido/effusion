defmodule Effusion.Repo.Migrations.AddDhtStateToTorrents do
  use Ecto.Migration

  def up do
    execute """
    alter type torrent_status add value if not exists 'dht_only'
    """
  end
end
