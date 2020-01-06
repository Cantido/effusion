defmodule Effusion.DHT.Bucket do
  use Ecto.Schema

  schema "buckets" do
    field :minimum, :binary, null: false
    field :maximum, :binary, null: false
    field :last_changed, :utc_datetime, null: false
  end
end
