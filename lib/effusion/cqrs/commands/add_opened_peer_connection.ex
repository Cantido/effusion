defmodule Effusion.CQRS.Commands.AddOpenedPeerConnection do
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
