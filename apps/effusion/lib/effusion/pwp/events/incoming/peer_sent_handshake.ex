defmodule Effusion.PWP.Events.Incoming.PeerSentHandshake do
  @derive Jason.Encoder
  @enforce_keys [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :initiated_by
  ]
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :initiated_by
  ]
end
