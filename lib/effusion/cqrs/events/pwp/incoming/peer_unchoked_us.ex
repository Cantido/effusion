defmodule Effusion.CQRS.Events.PeerUnchokedUs do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id
  ]
end
