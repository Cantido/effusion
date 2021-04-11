defmodule Effusion.PWP.Commands.Connection.RemoveConnectedPeer do
  @enforce_keys [
    :peer_uuid,
    :reason
  ]
  defstruct [
    :peer_uuid,
    :reason
  ]
end
