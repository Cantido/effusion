defmodule Effusion.CQRS.Commands.HandleChoke do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id
  ]
end
