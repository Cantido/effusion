defmodule Effusion.PWP.Events.Incoming.PeerSentBlock do
  @derive Jason.Encoder
  @enforce_keys [
    :peer_uuid,
    :info_hash,
    :index,
    :offset,
    :data
  ]
  defstruct [
    :peer_uuid,
    :info_hash,
    :index,
    :offset,
    :data
  ]
end