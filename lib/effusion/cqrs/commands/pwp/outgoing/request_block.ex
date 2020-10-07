defmodule Effusion.CQRS.Commands.RequestBlock do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :index,
    :offset,
    :size
  ]
end
