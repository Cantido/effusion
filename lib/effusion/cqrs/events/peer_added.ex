defmodule Effusion.CQRS.Events.PeerAdded do
  defstruct [
    :info_hash,
    :peer_id,
    :host,
    :port,
    :from
  ]
end
