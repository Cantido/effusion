defmodule Effusion.CQRS.Commands.HandleChoke do
  defstruct [
    :info_hash,
    :peer_id
  ]
end
