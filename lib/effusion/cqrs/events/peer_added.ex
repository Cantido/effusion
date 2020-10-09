defmodule Effusion.CQRS.Events.PeerAdded do
  @enforce_keys [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :host,
    :port,
    :from
  ]
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :host,
    :port,
    :from
  ]
end
