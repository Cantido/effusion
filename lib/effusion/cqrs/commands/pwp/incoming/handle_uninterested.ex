defmodule Effusion.CQRS.Commands.HandleUninterested do
  @enforce_keys [
    :peer_uuid
  ]
  defstruct [
    :peer_uuid
  ]
end
