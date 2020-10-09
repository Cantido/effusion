defmodule Effusion.CQRS.Commands.HandleInterested do
  @enforce_keys [
    :peer_uuid
  ]
  defstruct [
    :peer_uuid
  ]
end
