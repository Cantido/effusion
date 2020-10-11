defmodule Effusion.CQRS.Events.GettingPeers do
  @enforce_keys [
    :primary_node_id,
    :node_id,
    :host,
    :port,
    :info_hash
  ]
  defstruct [
    :primary_node_id,
    :node_id,
    :host,
    :port,
    :info_hash
  ]
end
