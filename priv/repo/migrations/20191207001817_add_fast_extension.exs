defmodule Effusion.Repo.Migrations.AddFastExtension do
  use Ecto.Migration

  def up do
    alter table(:peers) do
      add :fast_extension, :boolean, default: false, null: false
    end
  end

  def down do
    alter table(:peers) do
      remove :fast_extension
    end
  end
end
