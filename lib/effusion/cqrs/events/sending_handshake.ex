defmodule Effusion.CQRS.Events.SendingHandshake do
  @enforce_keys [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :our_peer_id,
    :our_extensions,
    :initiated_by
  ]
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :our_peer_id,
    :our_extensions,
    :initiated_by
  ]
end
