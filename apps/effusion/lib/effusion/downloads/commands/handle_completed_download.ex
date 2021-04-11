defmodule Effusion.Downloads.Commands.HandleCompletedDownload do
  @enforce_keys [
    :info_hash
  ]
  defstruct [
    :info_hash
  ]
end
