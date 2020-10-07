defmodule Effusion.CQRS.Events.SuccessfulHandshake do
  defstruct [
    :info_hash,
    :peer_id,
    :host,
    :port,
    :initiated_by
  ]
end
