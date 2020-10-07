defmodule Effusion.CQRS.Events.TorrentAdded do
  defstruct [
    :announce,
    :announce_list,
    :comment,
    :created_by,
    :creation_date,
    :info,
    :info_hash
  ]
end
