defmodule Effusion.CQRS.Commands.RefreshNode do
  defstruct [
    :node_id,
    :last_contacted
  ]
end
