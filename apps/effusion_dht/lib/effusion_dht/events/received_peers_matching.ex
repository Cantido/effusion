defmodule Effusion.CQRS.Events.ReceivedPeersMatching do
  @enforce_keys [
    :primary_node_id,
    :node_id,
    :transaction_id,
    :info_hash,
    :token,
    :peers
  ]
  defstruct [
    :primary_node_id,
    :node_id,
    :transaction_id,
    :info_hash,
    :token,
    :peers
  ]
end
