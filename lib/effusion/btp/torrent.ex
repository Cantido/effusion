defmodule Effusion.BTP.Torrent do
  alias Effusion.BTP.Piece
  alias Effusion.BTP.Block
  alias Effusion.BTP.Metainfo
  alias Effusion.Repo
  use Ecto.Schema
  import Ecto.Changeset
  require Logger

  @block_size Application.get_env(:effusion, :block_size)

  schema "torrents" do
    field :info_hash, :binary, null: false
    field :name, :string, null: false
    field :announce, :string, null: false
    field :started, :utc_datetime, null: true
    field :comment, :string, null: true
    field :created_by, :string, null: true
    field :creation_date, :utc_datetime, null: true
    field :trackerid, :string, null: true
    field :last_announce, :utc_datetime, null: true
    field :next_announce, :utc_datetime, null: true
  end

  @required_fields [:info_hash, :name, :announce]
  @optional_fields [
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

  def start(torrent, time) do
    Ecto.Changeset.change(torrent, started: DateTime.truncate(time, :second))
  end

  def insert(meta) do
    Metainfo.put_meta(meta)

    Repo.transaction(fn ->
      {:ok, torrent} = %__MODULE__{
        info_hash: meta.info_hash,
        name: meta.info.name,
        announce: meta.announce,
        comment: Map.get(meta, :comment),
        created_by: Map.get(meta, :created_by),
        creation_date: Map.get(meta, :creation_date) |> Timex.from_unix()
      } |> changeset()
      |> Repo.insert()

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
