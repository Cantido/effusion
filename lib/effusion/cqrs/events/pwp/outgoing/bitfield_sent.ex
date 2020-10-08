defmodule Effusion.CQRS.Events.BitfieldSent do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id,
    :bitfield
  ]
end
