defmodule Effusion.CQRS.Events.PeerHasPiece do
  defstruct [
    :info_hash,
    :index,
    :peer_id,
    :bitfield
  ]
end
