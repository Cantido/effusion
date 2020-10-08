defmodule Effusion.CQRS.Commands.AddConnectedPeer do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :host,
    :port,
    :initiated_by
  ]
end
