defmodule Effusion.DHT.NodeId do
  use Ecto.Type

  def type, do: :decimal

  def cast(val) when is_binary(val) do
    {:ok, val}
  end
  def cast(_), do: :error

  def load(decimal) do
    {:ok, <<Decimal.to_integer(decimal)::160>>}
  end

  def dump(<<val::160>>) do
    {:ok, Decimal.new(val)}
  end
  def dump(_), do: :error
end
