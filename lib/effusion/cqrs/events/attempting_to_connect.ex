defmodule Effusion.CQRS.Events.AttemptingToConnect do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :host,
    :port
  ]
end
