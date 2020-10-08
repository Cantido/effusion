defmodule Effusion.CQRS.Events.InterestedSent do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id
  ]
end
