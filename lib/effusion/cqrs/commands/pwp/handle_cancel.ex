defmodule Effusion.CQRS.Commands.HandleCancel do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id,
    :index,
    :offset,
    :size
  ]
end
