defmodule Effusion.CQRS.Commands.IssueToken do
  @enforce_keys [
    :node_id,
    :info_hash,
    :token,
    :expires_at
  ]
  defstruct [
    :node_id,
    :info_hash,
    :token,
    :expires_at
  ]
end
