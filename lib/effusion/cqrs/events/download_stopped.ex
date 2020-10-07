defmodule Effusion.CQRS.Events.DownloadStopped do
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
