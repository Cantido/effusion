defmodule Effusion.CQRS.Commands.HandleBitfield do
  defstruct [
    :info_hash,
    :peer_id,
    :bitfield
  ]
end
