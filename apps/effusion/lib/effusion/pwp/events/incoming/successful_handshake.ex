defmodule Effusion.PWP.Events.Incoming.SuccessfulHandshake do
  @derive Jason.Encoder
  @enforce_keys [
    :peer_uuid,
    :initiated_by
  ]
  defstruct [
    :peer_uuid,
    :initiated_by
  ]
end
