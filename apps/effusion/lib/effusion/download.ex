defmodule Effusion.Download do
  alias Effusion.Piece

  @enforce_keys [
    :meta
  ]
  defstruct [
    meta: nil,
    pieces: %{}
  ]

  def add_data(download, piece_index, offset, data) do
    new_piece =
      %Piece{
        index: piece_index,
        expected_hash: Enum.at(download.meta.info.pieces, piece_index),
        expected_size: Effusion.Metadata.piece_size(download.meta.info, piece_index)
      }
      |> Piece.add_data(offset, data)

    pieces = Map.update(download.pieces, piece_index, new_piece, &Piece.add_data(&1, offset, data))

    %__MODULE__{download | pieces: pieces}
  end
end
