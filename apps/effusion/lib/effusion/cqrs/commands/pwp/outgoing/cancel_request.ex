defmodule Effusion.CQRS.Commands.CancelRequest do
  @enforce_keys [
    :peer_uuid,
    :index,
    :offset,
    :size
  ]
  defstruct [
    :peer_uuid,
    :index,
    :offset,
    :size
  ]
end