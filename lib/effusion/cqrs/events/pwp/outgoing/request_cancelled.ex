defmodule Effusion.CQRS.Events.RequestCancelled do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id,
    :index,
    :offset,
    :size
  ]
end
