defmodule Effusion.PWP.Messages.Outgoing.Commands.SendHave do
  @enforce_keys [
    :peer_uuid,
    :index
  ]
  defstruct [
    :peer_uuid,
    :index
  ]
end
