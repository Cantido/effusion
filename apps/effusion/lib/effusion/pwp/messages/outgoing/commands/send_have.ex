defmodule Effusion.PWP.Commands.Outgoing.SendHave do
  @enforce_keys [
    :peer_uuid,
    :index
  ]
  defstruct [
    :peer_uuid,
    :index
  ]
end
