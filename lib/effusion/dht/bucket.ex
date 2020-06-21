defmodule Effusion.DHT.Bucket do
  use Ecto.Schema
  alias Effusion.DHT.Node
  import Ecto.Changeset
  import Ecto.Query
  import Effusion.DHT

  @moduledoc """
  A range of DHT node IDs.
  """

  schema "buckets" do
    has_many :nodes, Node
    field :range, Effusion.Numrange, null: false
    field :last_changed, :utc_datetime, null: false
  end

  def max_bucket_upper_value do
    # Bucket range upper limit is exclusive
    Node.max_node_id_value() + 1
  end

  def initial_bucket_range do
    [0, max_bucket_upper_value()]
  end

  def seed_bucket() do
    changeset(%__MODULE__{}, %{
      range: initial_bucket_range()
    })
  end

  def changeset(bucket, params \\ %{}) do
    bucket
    |> cast(params, [:range, :last_changed])
  end

  def for_node_id(<<node_id::160>>) do
    from bucket in __MODULE__,
    where: fragment("range @> ?::numeric", ^node_id)
  end

  def split(around) when is_node_id(around) do
    <<around_int::160>> = around
    result = Ecto.Adapters.SQL.query!(Effusion.Repo, "SELECT * FROM split_bucket($1);", [around_int])

    columns = Enum.map result.columns, &(String.to_atom(&1))

    Enum.map result.rows, fn(row) ->
      struct(__MODULE__, Enum.zip(columns, row))
    end
  end
end
