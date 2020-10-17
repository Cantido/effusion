defmodule Effusion.CQRS.Commands.TimeoutHandshake do
  @enforce_keys [
    :peer_uuid
  ]
  defstruct [
    :peer_uuid
  ]
end
