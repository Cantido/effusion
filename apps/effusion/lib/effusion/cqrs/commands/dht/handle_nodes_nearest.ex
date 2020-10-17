defmodule Effusion.CQRS.Commands.HandleNodesNearest do
  @enforce_keys [
    :transaction_id,
    :node_id,
    :token,
    :nodes
  ]
  defstruct [
    :transaction_id,
    :node_id,
    :token,
    :nodes
  ]
end
