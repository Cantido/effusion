defmodule Effusion.CQRS.Commands.HandleRequest do
  defstruct [
    :info_hash,
    :peer_id,
    :index,
    :offset,
    :size
  ]
end
