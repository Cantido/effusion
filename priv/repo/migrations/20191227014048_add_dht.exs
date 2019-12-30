defmodule Effusion.Repo.Migrations.AddDht do
  use Ecto.Migration

  def up do
    create table(:buckets) do
      add :minimum, :binary, null: false
      add :maximum, :binary, null: false
      add :last_changed, :utc_datetime, null: false
    end
    execute("ALTER TABLE buckets ALTER COLUMN last_changed SET DEFAULT CURRENT_TIMESTAMP")
    create constraint(:buckets, :minimum_must_be_twenty_bytes, check: "octet_length(minimum) = 20")
    create constraint(:buckets, :maximum_must_be_twenty_bytes, check: "octet_length(maximum) = 20")
    create constraint(:buckets, :minumum_must_be_less_than_maximum, check: "minimum < maximum")

    create table(:nodes) do
      add :bucket_id, references(:buckets, on_delete: :delete_all, on_update: :update_all), null: true
      add :node_id, :binary, null: false
      add :address, :inet, null: false
      add :port, :integer, null: false
      add :received_token, :binary, null: true
      add :sent_token, :binary, null: true
      add :sent_token_timestamp, :utc_datetime, null: true
      add :last_contacted, :utc_datetime, null: true
    end
    create unique_index(:nodes, [:address])
    create unique_index(:nodes, [:node_id, :address])
    create constraint(:nodes, :port_must_be_positive, check: "port > 0")
    create constraint(:nodes, :port_must_be_in_range, check: "port <= 65535")

    alter table(:torrents) do
      modify :name, :string, null: true
      modify :announce, :string, null: true
      modify :size, :integer, null: true
      modify :piece_size, :integer, null: true
    end
  end

  def down do
    drop table(:nodes)
    drop table(:buckets)

    alter table(:torrents) do
      modify :name, :string, null: false
      modify :announce, :string, null: false
      modify :size, :integer, null: false
      modify :piece_size, :integer, null: false
    end
  end
end
