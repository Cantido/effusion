defmodule Effusion.CQRS.Commands.StopDownload do
  defstruct [
    :info_hash,
    :tracker_event
  ]
end
