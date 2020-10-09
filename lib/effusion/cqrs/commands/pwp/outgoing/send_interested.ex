defmodule Effusion.CQRS.Commands.SendInterested do
  @enforce_keys [
    :peer_uuid
  ]
  defstruct [
    :peer_uuid
  ]
end
