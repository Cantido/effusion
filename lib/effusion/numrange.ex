defmodule Effusion.Numrange do
  @behaviour Ecto.Type

  def type, do: :numrange
  def embed_as(_), do: :self
  def equal?(left, right), do: left == right

  def cast([lower, upper]) do
    {:ok, [lower, upper]}
  end
  def cast(_), do: :error

  def load(%Postgrex.Range{lower: lower, upper: nil}) do
    {lower, _} = lower |> to_float
    {:ok, [lower, nil]}
  end
  def load(%Postgrex.Range{lower: lower, upper: upper}) do
    {lower, _} = lower |> to_float
    {upper, _} = upper |> to_float
    {:ok, [lower, upper]}
  end

  def dump([lower, upper]) do
    {:ok, %Postgrex.Range{lower: lower, upper: upper, upper_inclusive: false}}
  end
  def dump(_), do: :error

  defp to_float(value) do
   value |> Decimal.to_string |> Float.parse
  end
end
