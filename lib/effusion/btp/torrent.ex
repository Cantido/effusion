defmodule Effusion.BTP.Torrent do
  use Ecto.Schema
  alias Effusion.BTP.Peer
  alias Effusion.BTP.Piece
  alias Effusion.Repo
  import Effusion.Hash
  import Ecto.Changeset
  import Ecto.Query
  require Logger

  @moduledoc """
  A downloadable file or set of files.
  """

  schema "torrents" do
    field :info_hash, :binary, null: false
    field :name, :string, null: true
    field :state, :string, null: false, default: "paused"
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
    :state,
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

  def changeset(torrent = %__MODULE__{}, params \\ %{}) do
    # :started should be set only once
    params = if is_nil(torrent.started) do
      params
    else
      Map.drop(params, [:started])
    end

    torrent
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> validate_inclusion(:state, ["paused", "downloading", "finished", "dht_only"])
    |> unique_constraint(:info_hash)
  end

  def all do
    from torrent in __MODULE__, where: torrent.state != "dht_only"
  end

  def by_info_hash_query(info_hash) do
    from torrent in __MODULE__, where: torrent.info_hash == ^info_hash
  end

  def by_info_hash(info_hash) do
    torrent = Repo.one(by_info_hash_query(info_hash))
    if torrent != nil do
      {:ok, torrent}
    else
      {:error, "Torrent ID #{Effusion.Hash.encode info_hash} not found"}
    end
  end

  def by_info_hash!(info_hash) do
    Repo.one!(by_info_hash_query(info_hash))
  end

  def get(info_hash) when is_hash(info_hash) do
    from torrent in __MODULE__,
    where: torrent.info_hash == ^info_hash
  end

  def finished?(info_hash) when is_hash(info_hash) do
    case by_info_hash(info_hash) do
      {:ok, torrent} -> torrent.state == "finished"
      _ -> false
    end
  end

  def downloading?(info_hash) when is_hash(info_hash) do
    case by_info_hash(info_hash) do
      {:ok, torrent} -> torrent.state == "downloading"
      _ -> false
    end
  end
end
