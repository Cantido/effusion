defmodule Effusion.Repo.Migrations.InitialSchema do
  use Ecto.Migration

  def up do
    create table(:torrents) do
      add :info_hash, :binary, null: false
      add :name, :string, null: false
      add :announce, :string, null: false
      add :started, :utc_datetime, null: true
      add :comment, :string, null: true
      add :created_by, :string, null: true
      add :creation_date, :utc_datetime, null: true
    end
    create unique_index(:torrents, [:info_hash])
    create constraint(:torrents, :info_hash_must_be_twenty_bytes, check: "octet_length(info_hash) = 20")

    create table(:pieces) do
      add :torrent_id, references(:torrents, on_delete: :delete_all, on_update: :update_all), null: false
      add :index, :integer, null: false
      add :hash, :binary, null: false
      add :size, :integer, null: false
      add :verified, :boolean, default: false, null: false
      add :written, :boolean, default: false, null: false
    end
    create unique_index(:pieces, [:torrent_id, :index])
    create constraint(:pieces, :hash_must_be_twenty_bytes, check: "octet_length(hash) = 20")
    create constraint(:pieces, :index_must_be_positive, check: "index >= 0")
    create constraint(:pieces, :size_must_be_positive, check: "size >= 0")

    # Postgres doesn't like the column name "offset," so we name it "position" here
    create table(:blocks) do
      add :piece_id, references(:pieces, on_delete: :delete_all, on_update: :update_all), null: false
      add :position, :integer, null: false
      add :size, :integer, null: false
      add :data, :binary, null: true
    end
    create unique_index(:blocks, [:piece_id, :position])
    create constraint(:blocks, :offset_must_be_positive, check: "position >= 0")
    create constraint(:blocks, :size_must_be_positive, check: "size >= 0")

    create table(:peers) do
      add :peer_id, :binary, null: true
      add :address, :inet, null: false
      add :port, :integer, null: false
      add :failcount, :integer, null: false, default: 0
      add :peer_choking, :boolean, null: false, default: true
      add :peer_interested, :boolean, null: false, default: false
      add :am_choking, :boolean, null: false, default: true
      add :am_interested, :boolean, null: false, default: false
    end
    create unique_index(:peers, [:peer_id])
    create unique_index(:peers, [:address, :port])
    create constraint(:peers, :peer_id_must_be_twenty_bytes, check: "octet_length(peer_id) = 20")
    create constraint(:peers, :port_must_be_positive, check: "port > 0")
    create constraint(:peers, :port_must_be_in_range, check: "port <= 65535")

    create table(:peer_pieces) do
      add :peer_id, references(:peers, on_delete: :delete_all, on_update: :update_all), null: false
      add :piece_id, references(:pieces, on_delete: :delete_all, on_update: :update_all), null: false
    end
    create unique_index(:peer_pieces, [:peer_id, :piece_id])

    create table(:requests) do
      add :block_id, references(:blocks, on_delete: :delete_all, on_update: :update_all), null: false
      add :peer_id, references(:peers, on_delete: :delete_all, on_update: :update_all), null: false
    end
    create unique_index(:requests, [:peer_id, :block_id])
  end

  def down do
    drop table(:requests)
    drop table(:peer_pieces)
    drop table(:peers)
    drop table(:blocks)
    drop table(:pieces)
    drop table(:torrents)
  end
end
