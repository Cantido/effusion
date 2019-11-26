defmodule Effusion.BTP.AvailabilityMap do
  def new do
    Map.new()
  end

  @doc """
  Adds a piece to the availability map.

  ## Examples

      iex> Effusion.BTP.AvailabilityMap.add_piece(%{}, "Effusion Experiment!", 5) |> Map.get(5)
      #MapSet<["Effusion Experiment!"]>
  """
  @spec add_piece(map, binary, non_neg_integer) :: map
  def add_piece(avmap, peer_id, piece) do
    Map.update(avmap, piece, MapSet.new([peer_id]), &MapSet.put(&1, peer_id))
  end

  @doc """
  Removes a piece-peer combination from the availability map.
  """
  def remove_piece(avmap, peer_id, piece) do
    Map.update(avmap, piece, MapSet.new(), &MapSet.delete(&1, peer_id))
  end

  def remove_piece(avmap, piece) do
    Map.delete(avmap, piece)
  end

  def peers_with_block(avmap, blockid) do
    Map.get(avmap, blockid.index)
  end

  @doc """
  Removes a peer from the availability map.

  ## Examples

      iex> Effusion.BTP.AvailabilityMap.add_piece(%{}, "Effusion Experiment!", 5)
      ...> |> Effusion.BTP.AvailabilityMap.remove_peer("Effusion Experiment!")
      ...> |> Map.get(5)
      #MapSet<[]>
  """
  def remove_peer(avmap, peer_id) do
    Enum.map(avmap, fn {piece, peers} ->
      peers = MapSet.delete(peers, peer_id)
      {piece, peers}
    end)
    |> Map.new()
  end
end
