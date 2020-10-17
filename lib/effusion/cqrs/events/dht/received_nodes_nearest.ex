defmodule Effusion.CQRS.Events.ReceivedNodesNearest do
  @enforce_keys [
    :primary_node_id,
    :node_id,
    :transaction_id,
    :info_hash,
    :token,
    :nodes
  ]
  defstruct [
    :primary_node_id,
    :node_id,
    :transaction_id,
    :info_hash,
    :token,
    :nodes
  ]
end
