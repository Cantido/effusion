defmodule Effusion.CQRS.Commands.SendBitfield do
  defstruct [
    :peer_uuid,
    :bitfield
  ]
end
