defmodule Effusion.PWP.Messages.Incoming.Commands.HandleUnchoke do
  @enforce_keys [
    :peer_uuid
  ]
  defstruct [
    :peer_uuid
  ]
end
