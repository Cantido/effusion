defmodule Effusion.CQRS.Commands.RemoveConnectedPeer do
  @enforce_keys [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :reason
  ]
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :reason
  ]
end
