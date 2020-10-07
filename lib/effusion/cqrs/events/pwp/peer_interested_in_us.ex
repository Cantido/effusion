defmodule Effusion.CQRS.Events.PeerInterestedInUs do
  defstruct [
    :info_hash,
    :peer_id
  ]
end
