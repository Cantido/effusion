defmodule Effusion.PWP.Messages.Incoming.Commands.HandleBitfield do
  @enforce_keys [
    :peer_uuid,
    :bitfield
  ]
  defstruct [
    :peer_uuid,
    :bitfield
  ]
end
