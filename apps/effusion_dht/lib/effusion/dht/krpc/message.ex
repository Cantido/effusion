defmodule Effusion.DHT.KRPC.Message do
  @enforce_keys [:t, :y]
  defstruct [:t, :y, :v, :q, :a, :r, :e]
end
