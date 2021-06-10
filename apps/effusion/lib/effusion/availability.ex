defmodule Effusion.Availability do
  defstruct [
    pieces: %{}
  ]

  def peers_with_piece(avail, piece_index) when is_integer(piece_index) do
    Map.get(avail.pieces, piece_index)
  end

  def peer_pieces(avail, address) do
    Enum.filter(avail.pieces, fn {_index, addresses} ->
      MapSet.member?(addresses, address)
    end)
    |> Enum.map(&elem(&1, 0))
  end

  def peer_has_piece(avail, address, piece_index) when is_integer(piece_index) do
    pieces =
      Map.update(
        avail.pieces,
        piece_index,
        MapSet.new([address]),
        &MapSet.put(&1, address)
      )

    %{avail | pieces: pieces}
  end
end
