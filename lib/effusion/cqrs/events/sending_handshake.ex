defmodule Effusion.CQRS.Events.SendingHandshake do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :host,
    :port,
    :our_peer_id,
    :our_extensions,
    :initiated_by
  ]
end
