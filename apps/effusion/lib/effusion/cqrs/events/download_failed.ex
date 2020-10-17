defmodule Effusion.CQRS.Events.DownloadFailed do
  @derive Jason.Encoder
  @enforce_keys [
    :info_hash
  ]
  defstruct [
    :info_hash
  ]
end
