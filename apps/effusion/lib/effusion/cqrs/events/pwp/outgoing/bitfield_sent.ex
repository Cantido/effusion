defmodule Effusion.CQRS.Events.BitfieldSent do
  @derive Jason.Encoder
  @enforce_keys [
    :peer_uuid,
    :info_hash,
    :bitfield
  ]
  defstruct [
    :peer_uuid,
    :info_hash,
    :bitfield
  ]
end
