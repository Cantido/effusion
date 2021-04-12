defmodule Effusion.PWP.Messages.Incoming.Commands.HandleHave do
  @enforce_keys [
    :peer_uuid,
    :index
  ]
  defstruct [
    :peer_uuid,
    :index
  ]
end
