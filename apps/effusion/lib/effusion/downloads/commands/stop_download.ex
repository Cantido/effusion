defmodule Effusion.Downloads.Commands.StopDownload do
  @enforce_keys [
    :info_hash,
    :tracker_event
  ]
  defstruct [
    :info_hash,
    :tracker_event
  ]
end
