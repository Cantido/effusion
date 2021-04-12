defmodule Effusion.PWP.Swarm.Commands.AddPeerAddress do
  @enforce_keys [
    :peer_uuid,
    :expected_info_hash,
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
