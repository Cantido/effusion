defmodule Effusion.PWP.Commands.Incoming.HandleHave do
  @enforce_keys [
    :peer_uuid,
    :index
  ]
  defstruct [
    :peer_uuid,
    :index
  ]
end
