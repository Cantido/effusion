defmodule Effusion.PWP.Swarm.PeerProjection do
  use Ecto.Schema

  @primary_key {:id, Ecto.UUID, autogenerate: true}

  schema "peers" do
    field :peer_id, :binary
    field :info_hash, :binary, null: false
    field :host, :string, null: false
    field :port, :integer, null: false
  end
end
