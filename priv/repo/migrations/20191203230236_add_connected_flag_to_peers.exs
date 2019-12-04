defmodule Effusion.Repo.Migrations.AddConnectedFlagToPeers do
  use Ecto.Migration

  def up do
    alter table(:peers) do
      add :connected, :boolean, null: false, default: false
    end
  end

  def down do
    alter table(:peers) do
      remove :connected
    end
  end
end
