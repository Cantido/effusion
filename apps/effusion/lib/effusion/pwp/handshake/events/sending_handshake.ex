defmodule Effusion.PWP.Events.Handshake.SendingHandshake do
  @derive Jason.Encoder
  @enforce_keys [
    :peer_uuid,
    :info_hash,
    :our_peer_id,
    :our_extensions,
    :initiated_by
  ]
  defstruct [
    :peer_uuid,
    :info_hash,
    :our_peer_id,
    :our_extensions,
    :initiated_by
  ]
end
