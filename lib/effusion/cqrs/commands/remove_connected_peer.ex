defmodule Effusion.CQRS.Commands.RemoveConnectedPeer do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :host,
    :port,
    :reason
  ]
end
