defmodule Effusion.Repo.Migrations.AddNodes do
  use Ecto.Migration

  def change do
    create table("nodes") do
      add :node_id, :string, size: 40, primary_key: true
      add :host, :string, size: 45, null: false
      add :port, :integer, null: false
      add :last_heard_from, :utc_datetime_usec, null: false
    end

    unique_index("nodes", [:host, :port])
  end
end
