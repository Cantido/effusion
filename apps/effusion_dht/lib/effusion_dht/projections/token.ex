defmodule Effusion.CQRS.Projections.Token do
  use Ecto.Schema

  schema "tokens" do
    field :value, :string, size: 10, null: false
    field :issued_by, :string, size: 40, null: false
    field :issued_to, :string, size: 40, null: false
    field :info_hash, :string, size: 40, null: false
    field :expires_at, :utc_datetime_usec, null: false
  end
end
