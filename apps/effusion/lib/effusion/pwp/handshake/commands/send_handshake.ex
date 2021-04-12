defmodule Effusion.PWP.Handshake.Commands.SendHandshake do
  @enforce_keys [
    :peer_uuid,
    :our_peer_id,
    :our_extensions,
    :initiated_by
  ]
  defstruct [
    :peer_uuid,
    :our_peer_id,
    :our_extensions,
    :initiated_by
  ]
end
