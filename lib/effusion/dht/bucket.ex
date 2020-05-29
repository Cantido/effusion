defmodule Effusion.DHT.Bucket do
  use Ecto.Schema
  alias Effusion.DHT.Node

  @moduledoc """
  A range of DHT node IDs.
  """

  schema "buckets" do
    has_many :nodes, Node
    field :minimum, :binary, null: false
    field :maximum, :binary, null: false
    field :last_changed, :utc_datetime, null: false
  end
end
