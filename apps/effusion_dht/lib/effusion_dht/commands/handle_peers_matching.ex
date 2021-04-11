defmodule Effusion.CQRS.Commands.HandlePeersMatching do
  @enforce_keys [
    :transaction_id,
    :node_id,
    :token,
    :peers
  ]
  defstruct [
    :transaction_id,
    :node_id,
    :token,
    :peers
  ]
end
