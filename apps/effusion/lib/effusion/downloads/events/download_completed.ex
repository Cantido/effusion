defmodule Effusion.Downloads.Events.DownloadCompleted do
  @derive Jason.Encoder
  @enforce_keys [
    :info_hash
  ]
  defstruct [
    :info_hash
  ]
end
