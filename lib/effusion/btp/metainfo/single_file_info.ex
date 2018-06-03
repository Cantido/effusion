defmodule Effusion.BTP.Metainfo.SingleFileInfo do

  @enforce_keys [:length, :name, :piece_length, :pieces]
  defstruct [:length, :md5sum, :name, :piece_length, :pieces]

  def new(fields) do
    struct(Effusion.BTP.Metainfo.SingleFileInfo, update_info(fields))
  end

  defp update_info(info) when is_map(info) do
    info
    |> Effusion.Map.rename_keys(info_tokens())
    |> Map.update!(:pieces, &update_pieces/1)
  end

  defp info_tokens do
    %{
      "length" => :length,
      "name" => :name,
      "piece length" => :piece_length,
      "pieces" => :pieces
    }
  end

  defp update_pieces(pieces) when is_binary(pieces) do
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
end
