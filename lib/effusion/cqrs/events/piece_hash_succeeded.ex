defmodule Effusion.CQRS.Events.PieceHashSucceeded do
  @derive Jason.Encoder
  defstruct [
    :piece_id,
    :info_hash,
    :index,
    :data
  ]
end
