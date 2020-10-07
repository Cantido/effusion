defmodule Effusion.CQRS.Events.PeerHasBitfield do
  defstruct [
    :info_hash,
    :bitfield,
    :peer_id
  ]
end
