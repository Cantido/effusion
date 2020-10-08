defmodule Effusion.CQRS.Commands.HandleBitfield do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id,
    :bitfield
  ]
end
