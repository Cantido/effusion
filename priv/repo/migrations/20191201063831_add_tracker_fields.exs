defmodule Effusion.Repo.Migrations.AddTrackerFields do
  use Ecto.Migration

  def up do
    alter table("torrents") do
      add :trackerid, :string, null: true
      add :last_announce, :utc_datetime, null: true
      add :next_announce, :utc_datetime, null: true
    end
  end

  def down do
    alter table("torrents") do
      remove :trackerid
      remove :last_contacted
      remove :next_contact
    end
  end
end
