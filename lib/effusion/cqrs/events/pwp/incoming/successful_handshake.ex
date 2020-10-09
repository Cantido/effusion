defmodule Effusion.CQRS.Events.SuccessfulHandshake do
  @enforce_keys [
    :peer_uuid,
    :initiated_by
  ]
  defstruct [
    :peer_uuid,
    :initiated_by
  ]
end
