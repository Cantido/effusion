defmodule Effusion.Repo.Migrations.AddPeers do
  use Ecto.Migration

  def change do
    create table(:peers) do
      add :peer_id, :binary
      add :info_hash, :binary
      add :host, :string, null: false
      add :port, :integer, null: false
    end
  end
end
