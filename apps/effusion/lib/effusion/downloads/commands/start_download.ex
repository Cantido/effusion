defmodule Effusion.Downloads.Commands.StartDownload do
  @enforce_keys [
    :info_hash,
    :block_size,
    :max_requests_per_peer,
    :max_half_open_connections,
    :max_connections
  ]
  defstruct [
    :info_hash,
    :block_size,
    :max_requests_per_peer,
    :max_half_open_connections,
    :max_connections
  ]
end
