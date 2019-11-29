defmodule Effusion.BTP.Piece do
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.Block
  alias Effusion.BTP.PeerPiece
  alias Effusion.BTP.Request
  use Ecto.Schema
  import Ecto.Changeset
  import Effusion.Math

  schema "pieces" do
    belongs_to :torrent, Torrent
    field :index, :integer
    field :hash, :binary, null: false
    field :size, :integer, null: false
    has_many :blocks, Block
    has_many :peer_pieces, PeerPiece
    has_many :owners, through: [:peer_pieces, :peer]
  end

  def changeset(piece, params \\ %{}) do
    piece
    |> cast_assoc(:torrent)
    |> cast(params, [:index, :hash, :size])
    |> validate_required([:torrent, :index, :hash, :size])
  end

  def piece_size(index, info) do
    {whole_piece_count, last_piece_size} = divrem(info.length, info.piece_length)
    last_piece_index = whole_piece_count

    if index == last_piece_index do
      last_piece_size
    else
      info.piece_length
    end
  end
end
