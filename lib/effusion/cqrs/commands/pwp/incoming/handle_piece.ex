defmodule Effusion.CQRS.Commands.HandlePiece do
  defstruct [
    :peer_uuid,
    :index,
    :offset,
    :data
  ]
end
