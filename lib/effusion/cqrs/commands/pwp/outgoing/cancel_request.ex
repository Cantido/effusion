defmodule Effusion.CQRS.Commands.CancelRequest do
  defstruct [
    :peer_uuid,
    :index,
    :offset,
    :size
  ]
end
