defmodule Effusion.CQRS.Events.BlockRequested do
  defstruct [
    :info_hash,
    :index,
    :offset,
    :size
  ]
end
