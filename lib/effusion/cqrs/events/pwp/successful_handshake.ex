defmodule Effusion.CQRS.Events.SuccessfulHandshake do
  defstruct [
    :info_hash,
    :peer_id,
    :host,
    :port
  ]
end
