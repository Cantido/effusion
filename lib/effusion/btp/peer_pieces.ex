defmodule Effusion.BTP.PeerPiece do
  alias Effusion.BTP.Peer
  alias Effusion.BTP.Piece
  use Ecto.Schema

  schema "peer_pieces" do
    belongs_to :peer, Peer
    belongs_to :piece, Piece
  end
end
