defmodule Effusion.Repo.Migrations.AddTokens do
  use Ecto.Migration

  def change do
    create table("tokens") do
      add :value, :string, size: 10, null: false
      add :issued_to, :string, size: 40, null: false
      add :info_hash, :string, size: 40, null: false
      add :created_at, :utc_datetime_usec, null: false
    end

    create unique_index("tokens", [:value])
  end
end
