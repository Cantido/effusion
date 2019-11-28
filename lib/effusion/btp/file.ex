defmodule Effusion.BTP.File do
  alias Effusion.BTP.Torrent
  use Ecto.Schema

  schema "files" do
    belongs_to :torrent, Torrent
    field :length, :integer, null: false
    field :path, :string, null: false
  end
end
