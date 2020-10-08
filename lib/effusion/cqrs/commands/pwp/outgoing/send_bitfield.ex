defmodule Effusion.CQRS.Commands.SendBitfield do
  defstruct [
    :internal_peer_id,
    :bitfield
  ]
end
