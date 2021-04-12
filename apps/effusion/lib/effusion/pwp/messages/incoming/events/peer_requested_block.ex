defmodule Effusion.PWP.Messages.Incoming.Events.PeerRequestedBlock do
  @derive Jason.Encoder
  @enforce_keys [
    :peer_uuid,
    :info_hash,
    :index,
    :offset,
    :size
  ]
  defstruct [
    :peer_uuid,
    :info_hash,
    :index,
    :offset,
    :size
  ]
end
