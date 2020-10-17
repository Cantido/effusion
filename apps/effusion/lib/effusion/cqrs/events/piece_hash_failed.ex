defmodule Effusion.CQRS.Events.PieceHashFailed do
  @derive Jason.Encoder
  @enforce_keys [
    :info_hash,
    :index
  ]
  defstruct [
    :info_hash,
    :index
  ]
end
