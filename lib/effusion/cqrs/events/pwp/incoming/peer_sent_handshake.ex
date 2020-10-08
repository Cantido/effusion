defmodule Effusion.CQRS.Events.PeerSentHandshake do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :host,
    :port,
    :initiated_by,
    :extensions
  ]
end
