defmodule Effusion.PWP.Commands.Connection.DisconnectPeer do
  @enforce_keys [
    :peer_uuid
  ]
  defstruct [
    :peer_uuid
  ]
end