defmodule Effusion.CQRS.Commands.HandleCancel do
  defstruct [
    :info_hash,
    :peer_id,
    :index,
    :offset,
    :size
  ]
end
