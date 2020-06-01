defmodule Effusion.DHT.Bucket do
  use Ecto.Schema
  alias Effusion.DHT.Node
  import Ecto.Changeset
  import Ecto.Query

  @moduledoc """
  A range of DHT node IDs.
  """

  schema "buckets" do
    has_many :nodes, Node
    field :range, Effusion.Numrange, null: false
    field :last_changed, :utc_datetime, null: false
  end

  def changeset(bucket, params \\ %{}) do
    bucket
    |> cast(params, [:range, :last_changed])
  end

  def for_node_id(<<node_id::160>>) do
    from bucket in __MODULE__,
    where: fragment("range @> ?::numeric", ^node_id)
  end
end
