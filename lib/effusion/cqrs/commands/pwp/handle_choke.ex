defmodule Effusion.CQRS.Commands.HandleChoke do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id
  ]
end
