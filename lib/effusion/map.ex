defmodule Effusion.Map do
  def rename_keys(map, names) do
    for {key, val} <- map, into: %{}, do: {Map.get(names, key, key), val}
  end
end
