defmodule Effusion.CQRS.Events.PeerUninterestedInUs do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id
  ]
end
