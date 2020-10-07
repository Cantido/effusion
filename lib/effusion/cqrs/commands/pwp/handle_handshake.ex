defmodule Effusion.CQRS.Commands.HandleHandshake do
  defstruct [
    :info_hash,
    :peer_id,
    :host,
    :port
  ]
end
