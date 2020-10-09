defmodule Effusion.CQRS.Events.InterestedSent do
  @enforce_keys [
    :peer_uuid,
    :info_hash,
    :peer_id
  ]
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id
  ]
end
