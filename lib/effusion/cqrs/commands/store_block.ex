defmodule Effusion.CQRS.Commands.StoreBlock do
  defstruct [
    :from,
    :info_hash,
    :index,
    :offset,
    :data
  ]
end
