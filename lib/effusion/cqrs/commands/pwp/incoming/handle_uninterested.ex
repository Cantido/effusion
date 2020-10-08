defmodule Effusion.CQRS.Commands.HandleUninterested do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id
  ]
end
