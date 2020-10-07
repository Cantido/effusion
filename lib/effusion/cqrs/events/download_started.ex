defmodule Effusion.CQRS.Events.DownloadStarted do
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
  ]
end
