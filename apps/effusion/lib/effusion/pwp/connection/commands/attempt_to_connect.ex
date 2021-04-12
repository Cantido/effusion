defmodule Effusion.PWP.Connection.Commands.AttemptToConnect do
  @enforce_keys [
    :peer_uuid
  ]
  defstruct [
    :peer_uuid
  ]
end
