defmodule Effusion.CQRS.Projections.Node do
  use Ecto.Schema

  schema "nodes" do
    field(:node_id, :string)
    field(:host, :string)
    field(:port, :integer)
  end
end
