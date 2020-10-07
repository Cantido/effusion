defmodule Effusion.CQRS.Commands.HandleUninterested do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id
  ]
end
