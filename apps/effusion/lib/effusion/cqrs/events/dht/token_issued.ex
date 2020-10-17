defmodule Effusion.CQRS.Events.TokenIssued do
  @enforce_keys [
    :primary_node_id,
    :node_id,
    :info_hash,
    :token,
    :expires_at
  ]
  defstruct [
    :primary_node_id,
    :node_id,
    :info_hash,
    :token,
    :expires_at
  ]
end
