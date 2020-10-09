defmodule Effusion.CQRS.Events.InfoExpectedFromPeer do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id
  ]
end
