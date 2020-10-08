defmodule Effusion.CQRS.Events.BitfieldSent do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :bitfield
  ]
end
