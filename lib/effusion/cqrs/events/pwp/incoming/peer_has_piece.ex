defmodule Effusion.CQRS.Events.PeerHasPiece do
  @enforce_keys [
    :peer_uuid,
    :info_hash,
    :index,
    :bitfield
  ]
  defstruct [
    :peer_uuid,
    :info_hash,
    :index,
    :bitfield
  ]
end
