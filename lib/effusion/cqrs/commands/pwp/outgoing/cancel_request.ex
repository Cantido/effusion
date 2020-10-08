defmodule Effusion.CQRS.Commands.CancelRequest do
  defstruct [
    :internal_peer_id,
    :index,
    :offset,
    :size
  ]
end
