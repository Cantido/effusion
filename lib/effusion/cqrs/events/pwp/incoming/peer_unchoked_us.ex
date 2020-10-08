defmodule Effusion.CQRS.Events.PeerUnchokedUs do
  defstruct [
    :peer_uuid,
    :info_hash
  ]
end
