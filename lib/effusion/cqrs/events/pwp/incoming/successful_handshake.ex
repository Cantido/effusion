defmodule Effusion.CQRS.Events.SuccessfulHandshake do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :initiated_by
  ]
end
