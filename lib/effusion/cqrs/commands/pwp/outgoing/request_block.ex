defmodule Effusion.CQRS.Commands.RequestBlock do
  defstruct [
    :peer_uuid,
    :info_hash,
    :index,
    :offset,
    :size
  ]
end
