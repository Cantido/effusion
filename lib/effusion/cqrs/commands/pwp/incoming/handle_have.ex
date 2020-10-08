defmodule Effusion.CQRS.Commands.HandleHave do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id,
    :index
  ]
end
