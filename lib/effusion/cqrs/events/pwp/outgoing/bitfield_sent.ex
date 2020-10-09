defmodule Effusion.CQRS.Events.BitfieldSent do
  @enforce_keys [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :bitfield
  ]
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :bitfield
  ]
end
