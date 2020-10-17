defmodule Effusion.CQRS.Commands.GetPeers do
  @enforce_keys [
    :primary_node_id,
    :node_id,
    :info_hash
  ]
  defstruct [
    :primary_node_id,
    :node_id,
    :info_hash
  ]
end
