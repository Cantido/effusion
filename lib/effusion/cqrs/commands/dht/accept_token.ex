defmodule Effusion.CQRS.Commands.AcceptToken do
  defstruct [
    :node_id,
    :info_hash,
    :token,
    :expires_at
  ]
end
