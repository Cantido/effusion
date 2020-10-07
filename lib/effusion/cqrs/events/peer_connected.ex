defmodule Effusion.CQRS.Events.PeerConnected do
  defstruct [
    :info_hash,
    :peer_id,
    :host,
    :port
  ]
end
