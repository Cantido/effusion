defmodule Effusion.PWP.Messages.Incoming.Commands.HandleCancel do
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
