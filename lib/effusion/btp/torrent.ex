defmodule Effusion.BTP.Torrent do
  use Ecto.Schema
  import Ecto.Changeset

  schema "torrents" do
    field :info_hash, :binary, null: false
    field :name, :string, null: false
  end

  def changeset(torrent, params \\ %{}) do
    torrent
    |> cast(params, [:info_hash, :name])
    |> validate_required([:info_hash, :name])
    |> unique_constraint(:info_hash)
  end
end
