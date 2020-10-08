defmodule Effusion.CQRS.Events.AttemptingToConnect do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id,
    :host,
    :port
  ]
end
