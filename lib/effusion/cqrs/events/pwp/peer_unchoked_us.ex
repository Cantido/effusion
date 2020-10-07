defmodule Effusion.CQRS.Events.PeerUnchokedUs do
  defstruct [
    :info_hash,
    :peer_id
  ]
end
