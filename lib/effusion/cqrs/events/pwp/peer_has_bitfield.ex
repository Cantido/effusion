defmodule Effusion.CQRS.Events.PeerHasBitfield do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :bitfield,
    :peer_id
  ]
end
