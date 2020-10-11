defmodule Effusion.CQRS.Events.ConnectionAttemptFailed do
  @derive Jason.Encoder
  @enforce_keys [
    :peer_uuid,
    :info_hash
  ]
  defstruct [
    :peer_uuid,
    :info_hash
  ]
end
