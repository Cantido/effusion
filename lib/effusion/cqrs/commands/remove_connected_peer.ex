defmodule Effusion.CQRS.Commands.RemoveConnectedPeer do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id,
    :host,
    :port,
    :reason
  ]
end
