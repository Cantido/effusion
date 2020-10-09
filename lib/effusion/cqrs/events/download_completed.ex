defmodule Effusion.CQRS.Events.DownloadCompleted do
  @enforce_keys [
    :info_hash
  ]
  defstruct [
    :info_hash
  ]
end
