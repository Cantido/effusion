defmodule Effusion.PWP.Handshake.Commands.HandleHandshake do
  @enforce_keys [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :initiated_by,
    :extensions
  ]
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :initiated_by,
    :extensions
  ]
end
