defmodule Effusion.CQRS.Events.PeerDisconnected do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :reason
  ]
end
