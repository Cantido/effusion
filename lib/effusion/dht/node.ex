defmodule Effusion.DHT.Node do
  use Ecto.Schema
  import Ecto.Changeset

  schema "nodes" do
    field :node_id, :binary, null: false
    field :address, EctoNetwork.INET, null: false
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
    :received_token,
    :sent_token,
    :sent_token_timestamp,
    :last_contacted
  ]

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields ++ @optional_fields)
    |> update_change(:address, &wrap_address/1)
    |> update_change(:sent_token_timestamp, &truncate_timestamp/1)
    |> update_change(:last_contacted, &truncate_timestamp/1)
    |> validate_required(@required_fields)
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
