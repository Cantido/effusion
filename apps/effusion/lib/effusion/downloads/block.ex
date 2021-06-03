defmodule Effusion.Downloads.Block do
  defstruct [
    :offset,
    :data
  ]

  def size(%__MODULE__{data: data}) do
    byte_size(data)
  end
end
