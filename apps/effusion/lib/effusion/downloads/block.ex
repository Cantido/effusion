defmodule Effusion.Downloads.Block do
  @enforce_keys [
    :offset,
    :data
  ]
  defstruct [
    :offset,
    :data
  ]

  def size(%__MODULE__{data: data}) do
    byte_size(data)
  end
end
