defmodule Effusion.CQRS.Events.FailedHandshake do
  defstruct [
    :peer_uuid,
    :failure_reason
  ]
end
