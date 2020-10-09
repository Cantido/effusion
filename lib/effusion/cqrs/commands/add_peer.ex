defmodule Effusion.CQRS.Commands.AddPeer do
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
