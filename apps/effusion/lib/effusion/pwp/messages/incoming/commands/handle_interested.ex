defmodule Effusion.PWP.Messages.Incoming.Commands.HandleInterested do
  @enforce_keys [
    :peer_uuid
  ]
  defstruct [
    :peer_uuid
  ]
end
