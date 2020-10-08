defmodule Effusion.CQRS.Commands.HandleBitfield do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :bitfield
  ]
end
