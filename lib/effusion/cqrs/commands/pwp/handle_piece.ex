defmodule Effusion.CQRS.Commands.HandlePiece do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id,
    :index,
    :offset,
    :data
  ]
end
