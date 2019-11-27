defmodule Effusion.BTP.Request do
  use Ecto.Schema
  alias Effusion.BTP.{Block, Peer}

  schema "requests" do
    belongs_to :block, Block
    belongs_to :peer, Peer
  end
end
