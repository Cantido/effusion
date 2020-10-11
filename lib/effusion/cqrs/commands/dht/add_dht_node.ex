defmodule Effusion.CQRS.Commands.AddDHTNode do
  @enforce_keys [
    :primary_node_id,
    :node_id,
    :host,
    :port
  ]
  defstruct [
    :primary_node_id,
    :node_id,
    :host,
    :port
  ]
end
