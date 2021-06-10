defmodule Effusion.Availability do
  defstruct [
    pieces: %{}
  ]

  def peer_has_piece(avail, address, piece_index) do
    pieces =
      Map.update(
        avail.pieces,
        piece_index,
        MapSet.new([address]),
        &MapSet.put(&1, address)
      )

    %{avail | pieces: pieces}
  end

  def peers_with_piece(avail, piece_index) do
    Map.get(avail.pieces, piece_index)
  end
end
