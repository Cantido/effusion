defmodule Effusion.CQRS.Commands.HandleUnchoke do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id
  ]
end
