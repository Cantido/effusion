defmodule Effusion.PWP.Messages.Outgoing.Commands.SendBitfield do
  @enforce_keys [
    :peer_uuid,
    :bitfield
  ]
  defstruct [
    :peer_uuid,
    :bitfield
  ]
end
