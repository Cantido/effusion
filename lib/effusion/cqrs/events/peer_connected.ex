defmodule Effusion.CQRS.Events.PeerConnected do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :initiated_by
  ]
end
