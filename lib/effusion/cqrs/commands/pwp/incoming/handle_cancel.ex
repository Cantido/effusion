defmodule Effusion.CQRS.Commands.HandleCancel do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :index,
    :offset,
    :size
  ]
end
