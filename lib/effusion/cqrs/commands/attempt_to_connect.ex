defmodule Effusion.CQRS.Commands.AttemptToConnect do
  @enforce_keys [
    :peer_uuid
  ]
  defstruct [
    :peer_uuid
  ]
end
