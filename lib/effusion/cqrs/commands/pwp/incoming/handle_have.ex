defmodule Effusion.CQRS.Commands.HandleHave do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id,
    :index
  ]
end
