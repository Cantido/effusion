defmodule Effusion.CQRS.Events.PeerConnected do
  @enforce_keys [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :initiated_by
  ]
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :initiated_by
  ]
end
