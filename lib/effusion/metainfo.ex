defmodule Effusion.Metainfo do
  def decode(bin) do
    {:ok, decoded} = ExBencode.decode(bin)

    {:ok, info} = ExBencode.encode(decoded["info"])
    info_hash = :crypto.hash(:sha, info)

    {:ok, Map.put(decoded, :info_hash, info_hash)}
  end
end
