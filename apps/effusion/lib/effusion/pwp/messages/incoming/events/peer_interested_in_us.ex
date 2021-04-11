defmodule Effusion.PWP.Events.Incoming.PeerInterestedInUs do
  @derive Jason.Encoder
  @enforce_keys [
    :peer_uuid,
    :info_hash
  ]
  defstruct [
    :peer_uuid,
    :info_hash
  ]
end
