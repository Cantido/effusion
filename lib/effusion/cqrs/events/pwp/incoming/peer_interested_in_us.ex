defmodule Effusion.CQRS.Events.PeerInterestedInUs do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id
  ]
end
