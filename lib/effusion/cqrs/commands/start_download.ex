defmodule Effusion.CQRS.Commands.StartDownload do
  @enforce_keys [
    :info_hash,
    :block_size,
    :max_requests_per_peer
  ]
  defstruct [
    :info_hash,
    :block_size,
    :max_requests_per_peer
  ]
end
