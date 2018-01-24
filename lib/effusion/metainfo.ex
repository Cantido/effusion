defmodule Effusion.Metainfo do
  def decode(bin) do
    {:ok, decoded} = ExBencode.decode(bin)

    {:ok, info} = ExBencode.encode(decoded["info"])
    info_hash = :crypto.hash(:sha, info)

    result = decoded
      |> Map.put(:info_hash, info_hash)
      |> Effusion.Map.rename_keys(key_tokens())
      |> Map.update!(:info, &Effusion.Map.rename_keys(&1, info_tokens()))

    {:ok, result}
  end

  defp key_tokens do
    %{
      "announce" => :announce,
      "created by" => :created_by,
      "creation date" => :creation_date,
      "encoding" => :encoding,
      "info" => :info
    }
  end

  defp info_tokens do
    %{
      "length" => :length,
      "name" => :name,
      "piece length" => :piece_length
    }
  end
end
