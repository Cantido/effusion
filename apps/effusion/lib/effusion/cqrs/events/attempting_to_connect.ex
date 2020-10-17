defmodule Effusion.CQRS.Events.AttemptingToConnect do
  @derive Jason.Encoder
  @enforce_keys [
    :peer_uuid,
    :info_hash,
    :host,
    :port
  ]
  defstruct [
    :peer_uuid,
    :info_hash,
    :host,
    :port
  ]
end
