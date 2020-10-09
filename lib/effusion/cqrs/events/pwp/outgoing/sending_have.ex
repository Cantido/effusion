defmodule Effusion.CQRS.Events.SendingHave do
  @enforce_keys [
    :peer_uuid,
    :index
  ]
  defstruct [
    :peer_uuid,
    :index
  ]
end
