defmodule Effusion.CQRS.Events.PeerUninterestedInUs do
  defstruct [
    :peer_uuid,
    :info_hash
  ]
end
