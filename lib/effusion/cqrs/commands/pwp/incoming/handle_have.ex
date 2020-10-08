defmodule Effusion.CQRS.Commands.HandleHave do
  defstruct [
    :peer_uuid,
    :index
  ]
end
