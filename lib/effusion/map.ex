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

  @doc """
  Functions exactly like `Map.get_and_update/3`, but also takes a default value
  to use if the value at `key` is `nil`.

  The function argument `fun` *will* be run on the default argument.
  """
  def get_and_update(map, key, fun, default \\ nil) do
    Map.get_and_update(
      map,
      key,
      &nil_safe_call(fun, &1, default))
  end

  defp nil_safe_call(fun, nil, default) do
    fun.(default)
  end

  defp nil_safe_call(fun, arg, _default) do
    fun.(arg)
  end
end
