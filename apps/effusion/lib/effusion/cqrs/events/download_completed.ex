defmodule Effusion.CQRS.Events.DownloadCompleted do
  @derive Jason.Encoder
  @enforce_keys [
    :info_hash
  ]
  defstruct [
    :info_hash
  ]
end
