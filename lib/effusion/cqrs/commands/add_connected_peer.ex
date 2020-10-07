defmodule Effusion.CQRS.Commands.AddConnectedPeer do
  defstruct [
    :info_hash,
    :peer_id,
    :host,
    :port
  ]
end
