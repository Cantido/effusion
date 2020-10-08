defmodule Effusion.CQRS.Commands.HandleRequest do
  defstruct [
    :peer_uuid,
    :index,
    :offset,
    :size
  ]
end
