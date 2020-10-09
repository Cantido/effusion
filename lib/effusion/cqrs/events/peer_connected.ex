defmodule Effusion.CQRS.Events.PeerConnected do
  @enforce_keys [
    :peer_uuid,
    :info_hash,
    :initiated_by
  ]
  defstruct [
    :peer_uuid,
    :info_hash,
    :initiated_by
  ]
end
