defmodule Effusion.PWP.Commands.Outgoing.RequestBlock do
  @enforce_keys [
    :peer_uuid,
    :index,
    :offset,
    :size
  ]
  defstruct [
    :peer_uuid,
    :index,
    :offset,
    :size
  ]
end
