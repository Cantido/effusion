defmodule Effusion.CQRS.Events.PeerChokedUs do
  defstruct [
    :info_hash,
    :peer_id
  ]
end
