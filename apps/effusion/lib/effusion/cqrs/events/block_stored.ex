defmodule Effusion.CQRS.Events.BlockStored do
  @derive Jason.Encoder
  @enforce_keys [
    :from,
    :info_hash,
    :index,
    :offset,
    :data,
    :pieces
  ]
  defstruct [
    :from,
    :info_hash,
    :index,
    :offset,
    :data,
    :pieces
  ]
end
