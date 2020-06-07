defmodule Effusion.DHT.Node do
  use Ecto.Schema
  alias Effusion.DHT.Bucket
  alias Effusion.DHT.NodeId
  alias Timex.Duration
  import Ecto.Changeset

  @moduledoc """
  A peer in the DHT network.
  """

  schema "nodes" do
    belongs_to :bucket, Bucket
    field :address, EctoNetwork.INET, null: false
    field :node_id, NodeId, null: true
    field :port, :integer, null: false
    field :received_token, :binary, null: true
    field :sent_token, :binary, null: true
    field :sent_token_timestamp, :utc_datetime, null: true
    field :last_contacted, :utc_datetime, null: true
  end

  @required_fields [
    :node_id,
    :address,
    :port,
  ]
  @optional_fields [
    :bucket_id,
    :received_token,
    :sent_token,
    :sent_token_timestamp,
    :last_contacted
  ]

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields ++ @optional_fields)
    |> cast_assoc(:bucket, with: &Bucket.changeset/2)
    |> update_change(:address, &wrap_address/1)
    |> update_change(:sent_token_timestamp, &truncate_timestamp/1)
    |> update_change(:last_contacted, &truncate_timestamp/1)
    |> prepare_changes(&put_in_bucket/1)
    |> validate_required(@required_fields)
  end

  def max_node_id_value do
    trunc(:math.pow(2, 160)) - 1
  end

  def max_node_id_binary do
    <<max_node_id_value::160>>
  end

  def expired?(node, now) do
    latest_valid_time = Timex.subtract(now, Duration.from_minutes(15))
    Timex.before?(node.last_contacted, latest_valid_time)
  end

  defp put_in_bucket(changeset) do
    if node_id = get_change(changeset, :node_id) do
      bucket = Bucket.for_node_id(node_id) |> changeset.repo.one!()
      put_change(changeset, :bucket_id, bucket.id)
    else
      changeset
    end
  end

  defp wrap_address(address) when is_tuple(address) do
    %Postgrex.INET{address: address}
  end

  defp wrap_address(address = %Postgrex.INET{}) do
    address
  end

  defp truncate_timestamp(timestamp) do
    DateTime.truncate(timestamp, :second)
  end

  def compact(node) do
    id = node.node_id
    {ip0, ip1, ip2, ip3} = node.address.address
    port = node.port

    id <> <<ip0, ip1, ip2, ip3, port::16>>
  end
end
