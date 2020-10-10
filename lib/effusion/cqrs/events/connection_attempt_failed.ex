defmodule Effusion.CQRS.Events.ConnectionAttemptFailed do
  @derive Jason.Encoder
  @enforce_keys [
    :peer_uuid
  ]
  defstruct [
    :peer_uuid
  ]
end
