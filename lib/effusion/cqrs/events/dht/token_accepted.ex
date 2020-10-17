defmodule Effusion.CQRS.Events.TokenAccepted do
  defstruct [
    :primary_node_id,
    :node_id,
    :info_hash,
    :token,
    :expires_at
  ]
end
