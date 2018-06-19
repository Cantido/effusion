defmodule Effusion.Map do
  @moduledoc """
  Useful `Map` functions.
  """

  @doc """
  Rename the keys of `map` according to the rules given by `names`.

  ## Examples

      iex> Effusion.Map.rename_keys(%{a: 1, b: 2}, %{a: :x, b: :y})
      %{x: 1, y: 2}
  """
  def rename_keys(map, names) do
    for {key, val} <- map, into: %{}, do: {Map.get(names, key, key), val}
  end
end
