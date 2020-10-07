defmodule Effusion.CQRS.Events.PeerSentBlock do
  defstruct [
    :info_hash,
    :peer_id,
    :index,
    :offset,
    :data
  ]
end
