defmodule Effusion.CQRS.Commands.StartDownload do
  defstruct [
    :info_hash,
    :block_size
  ]
end
