defmodule Effusion.CQRS.Commands.SendInterested do
  defstruct [
    :peer_uuid,
    :info_hash
  ]
end
