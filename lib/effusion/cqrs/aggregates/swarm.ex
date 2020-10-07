defmodule Effusion.CQRS.Aggregates do
  defstruct [
    :info_hash,
    :connected_peers,
    :connecting_peers,
    :disconnected_peers
  ]
end
