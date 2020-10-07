defmodule Effusion.CQRS.Events.BlockStored do
  @derive Jason.Encoder
  defstruct [
    :piece_id,
    :from,
    :info_hash,
    :index,
    :offset,
    :data,
    :pieces
  ]
end
