defmodule Effusion.Repo.Migrations.AddTorrentPausedStates do
  use Ecto.Migration

  def up do
    execute """
    create type torrent_status as enum ('paused', 'downloading', 'finished')
    """

    alter table(:torrents) do
      add :state, :torrent_status, null: false, default: "paused"
    end
  end

  def down do
    alter table(:torrents) do
      remove :state
    end
    execute "drop type torrent_status"
  end
end
