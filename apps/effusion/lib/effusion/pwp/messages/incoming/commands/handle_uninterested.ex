defmodule Effusion.PWP.Messages.Incoming.Commands.HandleUninterested do
  @enforce_keys [
    :peer_uuid
  ]
  defstruct [
    :peer_uuid
  ]
end
