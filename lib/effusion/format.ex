defmodule Effusion.Format do
  import Effusion.Hash

  @kibibyte 1024
  @mebibyte 1024 * @kibibyte
  @gibibyte 1024 * @mebibyte
  @tebibyte 1024 * @gibibyte

  def bytes(n) when n >= 0 do
    cond do
      n < @kibibyte -> "#{n} B"
      n < @mebibyte -> "#{Float.round(n/@kibibyte, 1)} kiB"
      n < @gibibyte -> "#{Float.round(n/@mebibyte, 1)} MiB"
      n < @tebibyte -> "#{Float.round(n/@gibibyte, 1)} GiB"
      true -> "#{Float.round(n/@tebibyte, 1)} TiB"
    end
  end

  def info_hash(hash) when is_hash(hash) do
    Effusion.Hash.inspect(hash)
  end
end
