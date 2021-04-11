defmodule Effusion.PWP.Commands.Incoming.HandlePiece do
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
