defmodule Effusion.CQRS.Events.SuccessfulHandshake do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id,
    :host,
    :port,
    :initiated_by
  ]
end
