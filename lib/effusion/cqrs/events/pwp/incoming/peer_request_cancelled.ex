defmodule Effusion.CQRS.Events.PeerRequestCancelled do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :index,
    :peer_id,
    :offset,
    :size
  ]
end
