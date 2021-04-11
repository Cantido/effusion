defmodule Effusion.CQRS.Events.PeerHasBitfield do
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