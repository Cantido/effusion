defmodule Effusion.CQRS.Commands.HandleHandshake do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :host,
    :port,
    :initiated_by,
    :extensions
  ]
end
