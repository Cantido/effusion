defmodule Effusion.PWP.Events.Outgoing.SendingHave do
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
