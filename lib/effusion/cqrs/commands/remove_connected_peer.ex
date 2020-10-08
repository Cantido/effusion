defmodule Effusion.CQRS.Commands.RemoveConnectedPeer do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :reason
  ]
end
