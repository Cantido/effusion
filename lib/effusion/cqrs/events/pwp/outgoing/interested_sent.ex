defmodule Effusion.CQRS.Events.InterestedSent do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id
  ]
end
