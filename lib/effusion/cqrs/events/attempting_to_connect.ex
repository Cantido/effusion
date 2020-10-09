defmodule Effusion.CQRS.Events.AttemptingToConnect do
  @enforce_keys [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :host,
    :port
  ]
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :host,
    :port
  ]
end
