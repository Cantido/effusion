defmodule Effusion.Repo.Migrations.AddFilesToTorrent do
  use Ecto.Migration

  def up do
    alter table(:torrents) do
      add :size, :integer, null: false
      add :piece_size, :integer, null: false
    end
    create constraint(:torrents, :size_must_be_positive, check: "size >= 0")
    create constraint(:torrents, :piece_size_must_be_positive, check: "piece_size >= 0")

    create table(:files) do
      add :torrent_id, references(:torrents, on_delete: :delete_all, on_update: :update_all), null: false
      add :size, :integer, null: false
      add :path, :string, null: false
      add :index, :integer, null: false
      add :md5sum, :binary, null: true
    end

    create unique_index(:files, [:torrent_id, :path])
    create unique_index(:files, [:torrent_id, :index])
    create constraint(:files, :index_must_be_positive, check: "index >= 0")
    create constraint(:files, :size_must_be_positive, check: "size >= 0")
    create constraint(:files, :md5sum_must_be_sixteen_bytes, check: "octet_length(md5sum) = 16")
  end

  def down do
    drop table(:files)
    alter table(:torrents) do
      remove :size
      remove :piece_size
    end
  end
end
