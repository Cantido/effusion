defmodule Effusion.PWP.Connection.Commands.DisconnectPeer do
  @enforce_keys [
    :peer_uuid
  ]
  defstruct [
    :peer_uuid
  ]
end
