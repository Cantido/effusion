defmodule Effusion.CQRS.Events.PeerRequestCancelled do
  defstruct [
    :info_hash,
    :index,
    :peer_id,
    :offset,
    :size
  ]
end
