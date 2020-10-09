defmodule Effusion.CQRS.Events.InterestedSent do
  @enforce_keys [
    :peer_uuid,
    :info_hash
  ]
  defstruct [
    :peer_uuid,
    :info_hash
  ]
end
