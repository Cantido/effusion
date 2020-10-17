defmodule Effusion.CQRS.Commands.DisconnectPeer do
  @enforce_keys [
    :peer_uuid
  ]
  defstruct [
    :peer_uuid
  ]
end
