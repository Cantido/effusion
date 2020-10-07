defmodule Effusion.CQRS.Events.PeerDisconnected do
  defstruct [
    :info_hash,
    :peer_id,
    :host,
    :port
  ]
end
