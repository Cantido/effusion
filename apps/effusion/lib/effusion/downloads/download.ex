defmodule Effusion.Downloads.Download do
  @enforce_keys [
    :meta
  ]
  defstruct [
    meta: nil,
    pieces: %{}
  ]
end
