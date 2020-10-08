defmodule Effusion.CQRS.Commands.HandleHandshake do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :initiated_by,
    :extensions
  ]
end
