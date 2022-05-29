defmodule EffusionDesktop.Transfers do
  @moduledoc """
  The Transfers context.
  """

  def list_transfers do
    [
      get_transfer!(nil)
    ]
  end

  def get_transfer!(info_hash) do
    %Effusion.Torrent{
      id: "12345678901234567890",
      meta: %Metatorrent.Metainfo{
        info_hash: Base.decode16!("12345678901234567890"),
        announce: "https://example.org/announce",
        created_by: "Rosa :)",
        info: %Metatorrent.SingleFileInfo{
          name: "Some stupid movie (1997) [1080p]",
          length: 1024000000,
          piece_length: 1024000,
          pieces: 1..1000
        }
      }
    }
  end

  def create_transfer(attrs \\ %{}) do
    raise "TODO"
  end

  def update_transfer(transfer, attrs) do
    raise "TODO"
  end

  def delete_transfer(transfer) do
    raise "TODO"
  end
end
