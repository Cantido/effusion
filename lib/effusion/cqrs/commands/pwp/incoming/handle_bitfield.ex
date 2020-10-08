defmodule Effusion.CQRS.Commands.HandleBitfield do
  defstruct [
    :peer_uuid,
    :bitfield
  ]
end
