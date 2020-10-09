defmodule Effusion.CQRS.Commands.SendHave do
  defstruct [
    :peer_uuid,
    :index
  ]
end
