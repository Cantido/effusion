defmodule Effusion.BTP.Piece do
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.Block
  alias Effusion.BTP.PeerPiece
  alias Effusion.Repo
  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query
  import Effusion.Math

  schema "pieces" do
    belongs_to :torrent, Torrent
    field :index, :integer
    field :hash, :binary, null: false
    field :size, :integer, null: false
    field :announced, :boolean, default: false, null: false
    field :verified, :boolean, default: false, null: false
    field :written, :boolean, default: false, null: false
    has_many :blocks, Block
    has_many :peer_pieces, PeerPiece
    has_many :owners, through: [:peer_pieces, :peer]
  end

  def changeset(piece, params \\ %{}) do
    piece
    |> cast(params, [:index, :hash, :size, :written])
    |> validate_required([:torrent, :index, :hash, :size, :written])
  end

  def all_indicies_query(info_hash, indicies) do
    from p in __MODULE__,
      join: torrent in assoc(p, :torrent),
      where: torrent.info_hash == ^info_hash and p.index in ^indicies
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

  def get(info_hash, index) do
    from p in __MODULE__,
    join: torrent in assoc(p, :torrent),
    where: torrent.info_hash == ^info_hash,
    where: p.index == ^index
  end

  def has_all_blocks?(piece) do
    from piece in __MODULE__,
    join: block in assoc(piece, :blocks),
    where: piece.id == ^piece.id,
    group_by: piece.id,
    having: count(block.data) == count(block.id)
  end

  def verify(piece) do
    case Repo.one(has_all_blocks?(piece)) do
      nil -> piece
      piece ->
        piece_query =
          from piece in __MODULE__,
          join: block in assoc(piece, :blocks),
          where: piece.id == ^piece.id,
          group_by: piece.id,
          having: fragment(
            "digest(string_agg(?, '' ORDER BY ?), 'sha1')",
            block.data,
            block.offset
          ) == piece.hash
        case Repo.one!(piece_query) do
          nil -> piece
          piece -> Ecto.Changeset.change(piece, [verified: true]) |> Repo.update!()
        end
    end
  end
end
