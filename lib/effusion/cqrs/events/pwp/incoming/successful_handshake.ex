defmodule Effusion.CQRS.Events.SuccessfulHandshake do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :host,
    :port,
    :initiated_by
  ]
end
