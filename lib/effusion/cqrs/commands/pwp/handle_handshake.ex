defmodule Effusion.CQRS.Commands.HandleHandshake do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id,
    :host,
    :port,
    :initiated_by
  ]
end
