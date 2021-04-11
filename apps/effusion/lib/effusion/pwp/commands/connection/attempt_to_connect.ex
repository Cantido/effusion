defmodule Effusion.PWP.Commands.Connection.AttemptToConnect do
  @enforce_keys [
    :peer_uuid
  ]
  defstruct [
    :peer_uuid
  ]
end
