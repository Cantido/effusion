defmodule Effusion.PWP.Events.Handshake.FailedHandshake do
  @derive Jason.Encoder
  @enforce_keys [
    :peer_uuid,
    :failure_reason
  ]
  defstruct [
    :peer_uuid,
    :failure_reason
  ]
end
