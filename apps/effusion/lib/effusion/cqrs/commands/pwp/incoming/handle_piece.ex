defmodule Effusion.CQRS.Commands.HandlePiece do
  @enforce_keys  [
    :peer_uuid,
    :index,
    :offset,
    :data
  ]
  defstruct [
    :peer_uuid,
    :index,
    :offset,
    :data
  ]
end
