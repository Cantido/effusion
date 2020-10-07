defmodule Effusion.CQRS.Commands.AddConnectedPeer do
  defstruct [
    :info_hash,
    :peer_id,
    :host,
    :port,
    :initiated_by
  ]
end
