defmodule Effusion.CQRS.Events.BlockRequested do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :index,
    :offset,
    :size
  ]
end
