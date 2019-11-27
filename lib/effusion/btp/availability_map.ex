defmodule Effusion.BTP.AvailabilityMap do
  alias Effusion.BTP.Piece
  alias Effusion.BTP.Peer
  alias Effusion.BTP.PeerPiece
  alias Effusion.Repo
  import Ecto.Query

  def new do
    Map.new()
  end

  @doc """
  Adds a piece to the availability map.
  """
  @spec add_piece(map, binary, non_neg_integer) :: map
  def add_piece(_avmap, peer_id, piece) do
  
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

  def peers_with_block(avmap, block) do
    q = from p in Peer,
      join: pc in assoc(p, :pieces),
      join: b in assoc(pc, :blocks),
      where: b == ^block
    Repo.all(q)
  end

  @doc """
  Removes a peer from the availability map.
  """
  def remove_peer(avmap, peer_id) do
    Enum.map(avmap, fn {piece, peers} ->
      peers = MapSet.delete(peers, peer_id)
      {piece, peers}
    end)
    |> Map.new()
  end
end
