defmodule Effusion.CQRS.Events.PeerInterestedInUs do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id
  ]
end
