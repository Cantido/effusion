defmodule Effusion.Downloads.Events.PieceHashSucceeded do
  @derive Jason.Encoder
  @enforce_keys [
    :info_hash,
    :index,
    :data,
    :info
  ]
  defstruct [
    :info_hash,
    :index,
    :data,
    :info
  ]
end
