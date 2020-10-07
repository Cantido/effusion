defmodule Effusion.CQRS.Commands.HandleUninterested do
  defstruct [
    :info_hash,
    :peer_id
  ]
end
