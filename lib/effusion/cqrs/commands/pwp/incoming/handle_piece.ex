defmodule Effusion.CQRS.Commands.HandlePiece do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :index,
    :offset,
    :data
  ]
end
