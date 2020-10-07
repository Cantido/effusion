defmodule Effusion.CQRS.Commands.AddPeer do
  defstruct [
    :internal_peer_id,
    :info_hash,
    :peer_id,
    :host,
    :port,
    :from
  ]
end
