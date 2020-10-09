defmodule Effusion.CQRS.Events.PeerUninterestedInUs do
  @enforce_keys [
    :peer_uuid,
    :info_hash
  ]
  defstruct [
    :peer_uuid,
    :info_hash
  ]
end
