defmodule Effusion.CQRS.Commands.HandlePiece do
  defstruct [
    :info_hash,
    :peer_id,
    :index,
    :offset,
    :data
  ]
end
