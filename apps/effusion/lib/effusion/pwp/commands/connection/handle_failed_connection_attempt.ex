defmodule Effusion.PWP.Commands.Connection.HandleFailedConnectionAttempt do
  @enforce_keys [
    :peer_uuid,
    :reason
  ]
  defstruct [
    :peer_uuid,
    :reason
  ]
end
