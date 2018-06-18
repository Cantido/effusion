defmodule Effusion.BTP.Metainfo.SingleFileInfo do
  alias Effusion.BTP.Metainfo

  @enforce_keys [:length, :name, :piece_length, :pieces]
  defstruct [:length, :md5sum, :name, :piece_length, :pieces]

  def new(fields) do
    struct(Effusion.BTP.Metainfo.SingleFileInfo, update_info(fields))
  end

  defp update_info(info) when is_map(info) do
    info
    |> Effusion.Map.rename_keys(info_tokens())
    |> Map.update!(:pieces, &Metainfo.update_pieces/1)
  end

  defp info_tokens do
    %{
      "length" => :length,
      "name" => :name,
      "piece length" => :piece_length,
      "pieces" => :pieces
    }
  end
end
