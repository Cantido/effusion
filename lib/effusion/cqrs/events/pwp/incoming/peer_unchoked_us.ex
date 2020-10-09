defmodule Effusion.CQRS.Events.PeerUnchokedUs do
  @enforce_keys [
    :peer_uuid,
    :info_hash
  ]
  defstruct [
    :peer_uuid,
    :info_hash
  ]
end
