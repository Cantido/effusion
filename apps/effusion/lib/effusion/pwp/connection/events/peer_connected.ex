defmodule Effusion.PWP.Connection.Events.PeerConnected do
  @derive Jason.Encoder
  @enforce_keys [
    :peer_uuid,
    :info_hash,
    :initiated_by
  ]
  defstruct [
    :peer_uuid,
    :info_hash,
    :initiated_by
  ]
end
