defmodule Effusion.CQRS.Events.PeerChokedUs do
  defstruct [
    :peer_uuid,
    :info_hash
  ]
end
