defmodule Effusion.PWP.Commands.Incoming.HandleBitfield do
  @enforce_keys [
    :peer_uuid,
    :bitfield
  ]
  defstruct [
    :peer_uuid,
    :bitfield
  ]
end
