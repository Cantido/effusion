defmodule Effusion.BTP.Torrent do
  alias Effusion.BTP.File
  alias Effusion.BTP.Piece
  alias Effusion.BTP.Peer
  alias Effusion.BTP.Block
  alias Effusion.Repo
  use Ecto.Schema
  import Effusion.Hash
  import Ecto.Changeset
  import Ecto.Query
  require Logger

  @block_size Application.get_env(:effusion, :block_size)

  schema "torrents" do
    field :info_hash, :binary, null: false
    field :name, :string, null: true
    field :announce, :string, null: true
    field :size, :integer, null: true
    field :piece_size, :integer, null: true
    field :started, :utc_datetime, null: true
    field :comment, :string, null: true
    field :created_by, :string, null: true
    field :creation_date, :utc_datetime, null: true
    field :trackerid, :string, null: true
    field :last_announce, :utc_datetime, null: true
    field :next_announce, :utc_datetime, null: true
    has_many :pieces, Piece
    has_many :peers, Peer
  end

  @required_fields [
    :info_hash
  ]
  @optional_fields [
    :name,
    :announce,
    :size,
    :piece_size,
    :started,
    :comment,
    :created_by,
    :creation_date,
    :trackerid,
    :last_announce,
    :next_announce
  ]
  def changeset(torrent, params \\ %{}) do
    torrent
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> unique_constraint(:info_hash)
  end

  def by_info_hash(info_hash) do
    torrent_query = from torrent in __MODULE__,
                      where: torrent.info_hash == ^info_hash

    torrent = Repo.one(torrent_query)
    if torrent != nil do
      {:ok, torrent}
    else
      {:error, "Torrent ID #{Effusion.Hash.encode info_hash} not found"}
    end
  end

  def get(info_hash) when is_hash(info_hash) do
    from torrent in __MODULE__,
    where: torrent.info_hash == ^info_hash
  end

  def start(torrent, time) do
    Ecto.Changeset.change(torrent, started: DateTime.truncate(time, :second))
  end

  def insert(meta) do
    Repo.transaction(fn ->
      {:ok, torrent} = %__MODULE__{}
      |> changeset(%{
        info_hash: meta.info_hash,
        name: meta.info.name,
        size: meta.info.length,
        piece_size: meta.info.piece_length,
        announce: meta.announce,
        comment: Map.get(meta, :comment),
        created_by: Map.get(meta, :created_by),
        creation_date: Map.get(meta, :creation_date) |> Timex.from_unix()
      })
      |> Repo.insert()

      :ok = File.insert(meta, torrent)

      # Postgres can only accept 65535 parameters at a time,
      # so we need to chunk our inserts by 65535/params-per-entry

      IntSet.new()
      |> IntSet.inverse(meta.info.pieces |> Enum.count)
      |> Enum.map(fn index ->
          %{
            torrent_id: torrent.id,
            index: index,
            hash: Enum.at(meta.info.pieces, index),
            size: Piece.piece_size(index, meta.info)
          }
      end)
      # Each piece has four parameters, so we must insert in chunks of 65535/4 = 16383.75.
      |> Enum.chunk_every(16_383)
      |> Enum.reduce([], fn piece_entry_chunk, returned_pieces ->
        {_count, more_pieces} = Repo.insert_all(Piece, piece_entry_chunk, returning: true)
        more_pieces ++ returned_pieces
      end)
      |> Enum.flat_map(fn piece ->
        Enum.map(Block.split(piece, @block_size), fn block ->
            %{
              piece_id: piece.id,
              offset: block.offset,
              size: block.size
            }
        end)
      end)
      # Each block has three parameters, so we must insert in chunks of 65535/3 = 21345.
      |> Enum.chunk_every(21_845)
      |> Enum.each(fn block_entry_op ->
        Logger.debug("Attempting to insert #{Enum.count(block_entry_op)} blocks")
        Repo.insert_all(Block, block_entry_op)
      end)

      torrent
    end)
  end
end
