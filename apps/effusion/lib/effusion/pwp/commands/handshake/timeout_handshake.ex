defmodule Effusion.PWP.Commands.Handshake.TimeoutHandshake do
  @enforce_keys [
    :peer_uuid
  ]
  defstruct [
    :peer_uuid
  ]
end
