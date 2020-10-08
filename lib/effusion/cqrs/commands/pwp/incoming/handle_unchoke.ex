defmodule Effusion.CQRS.Commands.HandleUnchoke do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id
  ]
end
