defmodule Effusion.PWP.Messages.Outgoing.Events.SendingHave do
  @derive Jason.Encoder
  @enforce_keys [
    :peer_uuid,
    :index
  ]
  defstruct [
    :peer_uuid,
    :index
  ]
end
