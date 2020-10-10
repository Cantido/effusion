defmodule Effusion.CQRS.Events.TorrentAdded do
  @derive Jason.Encoder
  @enforce_keys [
    :announce,
    :announce_list,
    :comment,
    :created_by,
    :creation_date,
    :length,
    :piece_length,
    :name,
    :pieces,
    :files,
    :info_hash
  ]
  defstruct [
    :announce,
    :announce_list,
    :comment,
    :created_by,
    :creation_date,
    :length,
    :piece_length,
    :name,
    :pieces,
    :files,
    :info_hash
  ]
end
