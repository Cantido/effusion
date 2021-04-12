defmodule Effusion.PWP.Connection.Events.PeerConnectionOpened do
  @derive Jason.Encoder
  @enforce_keys [
    :peer_uuid,
    :host,
    :port
  ]
  defstruct [
    :peer_uuid,
    :host,
    :port
  ]
end
