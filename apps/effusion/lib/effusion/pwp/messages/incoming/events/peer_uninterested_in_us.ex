defmodule Effusion.PWP.Messages.Incoming.Events.PeerUninterestedInUs do
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
