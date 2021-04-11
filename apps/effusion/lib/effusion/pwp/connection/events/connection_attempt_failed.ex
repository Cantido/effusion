defmodule Effusion.PWP.Events.Connection.ConnectionAttemptFailed do
  @derive Jason.Encoder
  @enforce_keys [
    :peer_uuid,
    :info_hash,
    :reason
  ]
  defstruct [
    :peer_uuid,
    :info_hash,
    :reason
  ]
end