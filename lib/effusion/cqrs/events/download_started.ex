defmodule Effusion.CQRS.Events.DownloadStarted do
  @derive Jason.Encoder
  @enforce_keys [
    :announce,
    :announce_list,
    :info,
    :info_hash,
    :block_size,
    :bytes_uploaded,
    :bytes_downloaded,
    :bytes_left,
    :max_requests_per_peer
  ]
  defstruct [
    :announce,
    :announce_list,
    :info,
    :info_hash,
    :block_size,
    :bytes_uploaded,
    :bytes_downloaded,
    :bytes_left,
    :max_requests_per_peer
  ]
end
