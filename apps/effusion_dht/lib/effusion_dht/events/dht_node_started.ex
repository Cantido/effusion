defmodule Effusion.CQRS.Events.DHTNodeStarted do
  @enforce_keys [
    :node_id
  ]
  defstruct [
    :node_id
  ]
end
