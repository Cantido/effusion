defmodule Effusion.CQRS.Events.DownloadFailed do
  @enforce_keys [
    :info_hash
  ]
  defstruct [
    :info_hash
  ]
end
