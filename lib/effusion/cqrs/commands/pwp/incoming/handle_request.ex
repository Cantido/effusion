defmodule Effusion.CQRS.Commands.HandleRequest do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id,
    :index,
    :offset,
    :size
  ]
end
