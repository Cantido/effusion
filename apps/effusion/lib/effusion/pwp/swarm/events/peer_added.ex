defmodule Effusion.PWP.Swarm.Events.PeerAddressAdded do
  @derive Jason.Encoder
  @enforce_keys [
    :peer_uuid,
    :expected_info_hash,
    :expected_peer_id,
    :host,
    :port,
    :from
  ]
  defstruct [
    :peer_uuid,
    :expected_info_hash,
    :expected_peer_id,
    :host,
    :port,
    :from
  ]
end
