defmodule Effusion.Repo.Migrations.LinkPeersToTorrents do
  use Ecto.Migration

  def up do
    alter table(:peers) do
      add :torrent_id, references(:torrents, on_delete: :delete_all, on_update: :update_all), null: false
    end

    drop unique_index(:peers, [:peer_id])
    drop unique_index(:peers, [:address, :port])

    create unique_index(:peers, [:torrent_id, :peer_id])
    create unique_index(:peers, [:torrent_id, :address, :port])
  end

  def down do

    drop unique_index(:peers, [:torrent_id, :peer_id])
    drop unique_index(:peers, [:torrent_id, :address, :port])

    alter table(:peers) do
      remove :torrent_id
    end

    create unique_index(:peers, [:peer_id])
    create unique_index(:peers, [:address, :port])
  end
end
