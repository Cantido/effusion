defmodule Effusion.BTP.Metainfo do
  alias Effusion.Hash
  alias Effusion.BTP.Metainfo.SingleFileInfo
  alias Effusion.BTP.Metainfo.MultiFileInfo

  @enforce_keys [:info_hash, :announce, :created_by, :info]
  defstruct [:info_hash, :announce, :announce_list, :comment, :created_by, :creation_date, :info]

  def decode(bin) do
    {:ok, decoded} = ExBencode.decode(bin)

    {:ok, info} = ExBencode.encode(decoded["info"])
    info_hash = Hash.calc(info)

    result = decoded
      |> Map.put(:info_hash, info_hash)
      |> Effusion.Map.rename_keys(key_tokens())
      |> Map.update!(:info, &update_info/1)

    {:ok, struct(Effusion.BTP.Metainfo, result)}
  end

  defp update_info(info) do
    if Map.has_key?(info, "files") do
      MultiFileInfo.new(info)
    else
      SingleFileInfo.new(info)
    end
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

  def update_pieces(pieces) when is_binary(pieces) do
    binary_chunk(pieces)
  end

  defp binary_chunk(<<>>) do
    [<<>>]
  end

  defp binary_chunk(<<bin::binary-size(20)>>) do
    [bin]
  end

  defp binary_chunk(<<head::binary-size(20), rest::binary>>) when rem(byte_size(rest), 20) == 0 do
    [head | binary_chunk(rest)]
  end

  defimpl Inspect, for: Effusion.BTP.Metainfo.SingleFileInfo do
    import Inspect.Algebra

    def inspect(info, opts) do
      concat([
        "#SingleFileInfo<[",
        to_doc(info.name, opts),
        "]>"
      ])
    end
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
