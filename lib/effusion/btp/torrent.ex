defmodule Effusion.BTP.Torrent do
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.Piece
  alias Effusion.BTP.Block
  alias Effusion.Repo
  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query
  import Effusion.Math

  schema "torrents" do
    field :info_hash, :binary, null: false
    field :name, :string, null: false
    field :started, :utc_datetime, null: false
    field :announce, :string, null: false
    field :created_at, :utc_datetime, null: true
    field :created_by, :string, null: true
    field :encoding, :string, null: true
    field :comment, :string, null: true
  end

  @block_size Application.get_env(:effusion, :block_size)
  @required_fields [
    :info_hash,
    :name,
    :started,
    :announce
  ]
  @optional_fields [
    :created_at,
    :created_by,
    :encoding,
    :comment
  ]
  def changeset(torrent, params \\ %{}) do
    torrent
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> unique_constraint(:info_hash)
  end

  def insert(meta) do
    info_hash = meta.info_hash
    torrent = Repo.one(from t in Torrent, where: t.info_hash == ^info_hash)

    if is_nil(torrent) do
      {:ok, torrent} =
        Torrent.changeset(%Torrent{},
          Map.merge(meta, %{
            name: meta.info.name,
            started: Timex.now() |> DateTime.truncate(:second)}))
      |> Repo.insert()

        Map.merge(meta, %{
          name: meta.info.name,
          started: Timex.now() |> DateTime.truncate(:second)})

      IntSet.new()
      |> IntSet.inverse(meta.info.pieces |> Enum.count)
      |> Enum.each(fn index ->
        {:ok, piece} =
          %Piece{
            torrent: torrent,
            index: index,
            hash: Enum.at(meta.info.pieces, index),
            size: piece_size(index, meta.info),
            blocks: []
          }
          |> Repo.insert()

        Enum.each(Block.split(piece, @block_size), fn block ->
            %Block{
              piece: piece,
              offset: block.offset,
              size: block.size
            } |> Repo.insert()
        end)
      end)

      if Map.has_key?(meta.info, :files) do
        Enum.each(meta.info.files, fn file ->
          {:ok, file} = %Effusion.BTP.File{
            torrent: torrent,
            length: file.length,
            path: Path.join(file.path)
          } |> Repo.insert()
        end)
      else
        %Effusion.BTP.File{
          torrent: torrent,
          length: meta.info.length,
          path: meta.info.name
        }
      end
      torrent
    else
      torrent
    end
  end

  defp piece_size(index, info) do
    {whole_piece_count, last_piece_size} = divrem(info.length, info.piece_length)
    last_piece_index = whole_piece_count

    if index == last_piece_index do
      last_piece_size
    else
      info.piece_length
    end
  end
end
