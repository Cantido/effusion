defmodule Effusion.CQRS.Commands.StoreBlock do
  @enforce_keys [
    :from,
    :info_hash,
    :index,
    :offset,
    :data
  ]
  defstruct [
    :from,
    :info_hash,
    :index,
    :offset,
    :data
  ]
end
