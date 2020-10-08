defmodule Effusion.CQRS.Commands.HandleInterested do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id
  ]
end
