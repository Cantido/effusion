defmodule Effusion.DHT.Node do
  use Ecto.Schema

  schema "nodes" do
    field :node_id, :binary, null: false
    field :address, EctoNetwork.INET, null: false
    field :port, :integer, null: false
    field :received_token, :binary, null: true
    field :sent_token, :binary, null: true
    field :sent_token_timestamp, :utc_datetime, null: true
    field :last_contacted, :utc_datetime, null: false
  end

  def compact(node) do
    id = node.node_id
    {ip0, ip1, ip2, ip3} = node.address.address
    port = node.port

    id <> <<ip0, ip1, ip2, ip3, port::16>>
  end
end
