defmodule Effusion.CQRS.Events.PeerHasBitfield do
  defstruct [
    :peer_uuid,
    :info_hash,
    :bitfield
  ]
end
