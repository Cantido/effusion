defmodule Effusion.CQRS.Commands.RemoveConnectedPeer do
  defstruct [
    :info_hash,
    :peer_id,
    :host,
    :port,
    :reason
  ]
end
