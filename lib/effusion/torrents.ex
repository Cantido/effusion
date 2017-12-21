defmodule Effusion.Torrents do
  def lookup(info_hash) do
    case info_hash do
      <<0 :: size(160)>> -> :ok
      _ -> :not_found
    end
  end
end
