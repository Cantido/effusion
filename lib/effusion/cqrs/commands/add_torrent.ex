defmodule Effusion.CQRS.Commands.AddTorrent do
  @enforce_keys [
    :announce,
    :announce_list,
    :comment,
    :created_by,
    :creation_date,
    :info,
    :info_hash
  ]
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
