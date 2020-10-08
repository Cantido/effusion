defmodule Effusion.CQRS.Events.PeerAdded do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :host,
    :port,
    :from
  ]
end
