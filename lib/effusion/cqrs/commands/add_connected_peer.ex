defmodule Effusion.CQRS.Commands.AddConnectedPeer do
  @enforce_keys [
    :peer_uuid,
    :initiated_by
  ]
  defstruct [
    :peer_uuid,
    :initiated_by
  ]
end
