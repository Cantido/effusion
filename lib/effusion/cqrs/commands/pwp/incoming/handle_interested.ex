defmodule Effusion.CQRS.Commands.HandleInterested do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id
  ]
end
