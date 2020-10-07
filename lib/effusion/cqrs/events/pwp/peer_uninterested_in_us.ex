defmodule Effusion.CQRS.Events.PeerUninterestedInUs do
  defstruct [
    :info_hash,
    :peer_id
  ]
end
