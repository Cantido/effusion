defmodule Effusion.PWP.Connection.Commands.AddOpenedPeerConnection do
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
