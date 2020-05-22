defmodule Effusion.Repo.Migrations.MakeNodeIdNullable do
  use Ecto.Migration

  def up do
    alter table(:nodes) do
      modify :node_id, :binary, null: true
    end
  end

  def down do
    alter table(:nodes) do
      modify :node_id, :binary, null: false
    end
  end
end
