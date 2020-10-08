defmodule Effusion.CQRS.Events.PeerRequestCancelled do
  defstruct [
    :peer_uuid,
    :info_hash,
    :index,
    :peer_id,
    :offset,
    :size
  ]
end
