defmodule Effusion.CQRS.Commands.HandleHave do
  defstruct [
    :info_hash,
    :peer_id,
    :index
  ]
end
