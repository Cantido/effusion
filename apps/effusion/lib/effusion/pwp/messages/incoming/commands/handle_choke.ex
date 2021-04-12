defmodule Effusion.PWP.Messages.Incoming.Commands.HandleChoke do
  @enforce_keys [
    :peer_uuid
  ]
  defstruct [
    :peer_uuid
  ]
end
