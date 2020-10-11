defmodule Effusion.CQRS.Commands.StartDHTNode do
  @enforce_keys [
    :node_id
  ]
  defstruct [
    :node_id
  ]
end
