defmodule Effusion.CQRS.Events.PeerChokedUs do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id
  ]
end
