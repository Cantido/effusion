defmodule Effusion.CQRS.Events.NodeRefreshed do
  @enforce_keys [
    :node_id,
    :last_contacted
  ]
  defstruct [
    :node_id,
    :last_contacted
  ]
end
