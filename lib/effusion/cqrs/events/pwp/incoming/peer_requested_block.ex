defmodule Effusion.CQRS.Events.PeerRequestedBlock do
  defstruct [
    :peer_uuid,
    :info_hash,
    :index,
    :offset,
    :size
  ]
end
