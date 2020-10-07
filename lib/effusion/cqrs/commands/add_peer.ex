defmodule Effusion.CQRS.Commands.AddPeer do
  defstruct [
    :info_hash,
    :peer_id,
    :host,
    :port
  ]
end
