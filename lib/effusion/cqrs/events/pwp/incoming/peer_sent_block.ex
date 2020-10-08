defmodule Effusion.CQRS.Events.PeerSentBlock do
  defstruct [
    :peer_uuid,
    :info_hash,
    :index,
    :offset,
    :data
  ]
end
