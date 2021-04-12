defmodule Effusion.PWP.Messages.Outgoing.Commands.SendInterested do
  @enforce_keys [
    :peer_uuid
  ]
  defstruct [
    :peer_uuid
  ]
end
