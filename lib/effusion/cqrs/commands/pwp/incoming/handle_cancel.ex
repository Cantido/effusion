defmodule Effusion.CQRS.Commands.HandleCancel do
  defstruct [
    :peer_uuid,
    :index,
    :offset,
    :size
  ]
end
