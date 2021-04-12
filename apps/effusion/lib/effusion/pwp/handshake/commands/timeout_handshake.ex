defmodule Effusion.PWP.Handshake.Commands.TimeoutHandshake do
  @enforce_keys [
    :peer_uuid
  ]
  defstruct [
    :peer_uuid
  ]
end
