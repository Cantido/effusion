defmodule Effusion.CQRS.Events.PeerRequestedBlock do
  defstruct [
    :info_hash,
    :peer_id,
    :index,
    :offset,
    :size
  ]
end
