defmodule Effusion.BTP.PeerPiece do
  alias Effusion.BTP.Peer
  alias Effusion.BTP.Piece
  use Ecto.Schema

  @moduledoc """
  A piece that a peer has.

  We must keep track of which peers have which pieces
  in order to know what data we can request from them,
  and also to calculate the rarity of pieces and
  the overall availability of a torrent download.
  """

  schema "peer_pieces" do
    belongs_to :peer, Peer
    belongs_to :piece, Piece
  end
end
