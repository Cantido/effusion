defmodule Effusion.CQRS.Commands.StartDownload do
  @enforce_keys [
    :info_hash,
    :block_size
  ]
  defstruct [
    :info_hash,
    :block_size
  ]
end
