defmodule Effusion.CQRS.Events.PeerCancelledRequest do
  @enforce_keys [
    :peer_uuid,
    :info_hash,
    :index,
    :offset,
    :size
  ]
  defstruct [
    :peer_uuid,
    :info_hash,
    :index,
    :offset,
    :size
  ]
end
