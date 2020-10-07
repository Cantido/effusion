defmodule Effusion.CQRS.Events.PieceHashFailed do
  @derive Jason.Encoder
  defstruct [
    :info_hash,
    :index
  ]
end
