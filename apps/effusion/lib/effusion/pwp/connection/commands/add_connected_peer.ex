defmodule Effusion.PWP.Connection.Commands.AddConnectedPeer do
  @enforce_keys [
    :peer_uuid,
    :initiated_by
  ]
  defstruct [
    :peer_uuid,
    :initiated_by
  ]
end
