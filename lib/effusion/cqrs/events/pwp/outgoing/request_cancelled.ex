defmodule Effusion.CQRS.Events.RequestCancelled do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :index,
    :offset,
    :size
  ]
end
