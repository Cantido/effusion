defmodule Effusion.CQRS.Commands.HandleRequest do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :index,
    :offset,
    :size
  ]
end
