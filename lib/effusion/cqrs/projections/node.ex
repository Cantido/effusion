defmodule Effusion.CQRS.Projections.Node do
  use Ecto.Schema

  schema "nodes" do
    field(:node_id, :string)
    field(:host, :string)
    field(:port, :integer)
    field(:last_heard_from, :utc_datetime_usec)
  end
end
