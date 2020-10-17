defmodule Effusion.CQRS.Events.DownloadStopped do
  @derive Jason.Encoder
  @enforce_keys [
    :announce,
    :announce_list,
    :comment,
    :created_by,
    :creation_date,
    :info,
    :info_hash,
    :bytes_uploaded,
    :bytes_downloaded,
    :bytes_left,
    :tracker_event
  ]
  defstruct [
    :announce,
    :announce_list,
    :comment,
    :created_by,
    :creation_date,
    :info,
    :info_hash,
    :bytes_uploaded,
    :bytes_downloaded,
    :bytes_left,
    :tracker_event
  ]
end
