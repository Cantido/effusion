defmodule Effusion.BTP.Metainfo do
  alias Effusion.Hash
  alias Effusion.BTP.Metainfo.SingleFileInfo

  @enforce_keys [:info_hash, :announce, :created_by, :info]
  defstruct [:info_hash, :announce, :announce_list, :comment, :created_by, :creation_date, :info]

  def decode(bin) do
    {:ok, decoded} = ExBencode.decode(bin)

    {:ok, info} = ExBencode.encode(decoded["info"])
    info_hash = Hash.calc(info)

    result = decoded
      |> Map.put(:info_hash, info_hash)
      |> Effusion.Map.rename_keys(key_tokens())
      |> Map.update!(:info, &SingleFileInfo.new/1)

    {:ok, struct(Effusion.BTP.Metainfo, result)}
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

  defimpl Inspect, for: Effusion.BTP.Metainfo do
    import Inspect.Algebra

    def inspect(meta, opts) do
      concat([
        "#Metainfo<[",
        to_doc(meta.info.name, opts),
        break(),
        Hash.inspect(meta.info_hash),
        "]>"
      ])
    end
  end
end
