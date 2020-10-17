defmodule Effusion.CQRS.Commands.HandleFailedConnectionAttempt do
  @enforce_keys [
    :peer_uuid,
    :reason
  ]
  defstruct [
    :peer_uuid,
    :reason
  ]
end
