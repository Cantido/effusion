defmodule Effusion.CQRS.Events.PeerHasPiece do
  defstruct [
    :peer_uuid,
    :info_hash,
    :index,
    :peer_id,
    :bitfield
  ]
end
