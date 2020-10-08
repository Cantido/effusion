defmodule Effusion.CQRS.Commands.SendHandshake do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :our_peer_id,
    :our_extensions,
    :initiated_by
  ]
end
