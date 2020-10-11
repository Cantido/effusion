defmodule Effusion.CQRS.Commands.EnableDHTForDownload do
  @enforce_keys [
    :info_hash,
    :node_id
  ]
  defstruct [
    :info_hash,
    :node_id
  ]
end
