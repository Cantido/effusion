defmodule Effusion.CQRS.Commands.SendInterested do
  defstruct [
    :internal_peer_id,
    :info_hash
  ]
end
