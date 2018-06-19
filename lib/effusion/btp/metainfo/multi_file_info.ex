defmodule Effusion.BTP.Metainfo.MultiFileInfo do
  alias Effusion.BTP.Metainfo

  @moduledoc """
  A metadata file's `:info` block describing mutliple downloadable files.
  """

  @enforce_keys [:files, :name, :piece_length, :pieces]
  defstruct [:files, :name, :piece_length, :pieces, :length]

  def new(fields) do
    struct(Effusion.BTP.Metainfo.MultiFileInfo, update_info(fields))
  end

  defp update_info(info) when is_map(info) do
    info
    |> Effusion.Map.rename_keys(info_tokens())
    |> Map.update!(:pieces, &Metainfo.update_pieces/1)
    |> Map.update!(:files, &update_files/1)
    |> put_total_length()
  end

  defp put_total_length(info) do
    Map.put(info, :length, total_length(info))
  end

  defp total_length(info) do
    info.files
    |> Enum.map(&(&1.length))
    |> Enum.sum()
  end

  defp update_files(files) do
    Enum.map(files, &update_file/1)
  end

  defp update_file(file) do
    Effusion.Map.rename_keys(file, file_tokens())
  end

  defp info_tokens do
    %{
      "files" => :files,
      "name" => :name,
      "piece length" => :piece_length,
      "pieces" => :pieces
    }
  end

  defp file_tokens do
    %{
      "length" => :length,
      "path" => :path
    }
  end
end
