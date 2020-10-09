defmodule Effusion.CQRS.Events.PeerDisconnected do
  @enforce_keys [
    :peer_uuid,
    :info_hash,
    :reason
  ]
  defstruct [
    :peer_uuid,
    :info_hash,
    :reason
  ]
end
