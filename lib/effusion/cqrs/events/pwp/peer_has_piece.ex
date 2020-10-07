defmodule Effusion.CQRS.Events.PeerHasPiece do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :index,
    :peer_id,
    :bitfield
  ]
end
