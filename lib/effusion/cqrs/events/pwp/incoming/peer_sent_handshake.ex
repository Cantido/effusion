defmodule Effusion.CQRS.Events.PeerSentHandshake do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id,
    :host,
    :port,
    :initiated_by,
    :extensions
  ]
end
