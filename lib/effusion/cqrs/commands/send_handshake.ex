defmodule Effusion.CQRS.Commands.SendHandshake do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id,
    :host,
    :port,
    :our_peer_id,
    :our_extensions,
    :initiated_by
  ]
end
