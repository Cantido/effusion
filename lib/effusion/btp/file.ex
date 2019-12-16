defmodule Effusion.BTP.File do
  alias Effusion.BTP.Torrent
  alias Effusion.Repo
  import Ecto.Changeset
  use Ecto.Schema

  schema "files" do
    belongs_to :torrent, Torrent
    field :size, :integer, null: false
    field :path, :string, null: false
    field :index, :integer, null: false
    field :md5sum, :binary, null: true
  end

  def changeset(file, params \\ {}) do
    file
    |> cast(params, [:size, :path, :md5sum])
    |> validate_required([:size, :path])
  end


  def insert(meta, torrent) do
    if Map.has_key?(meta.info, :files) do
      to_insert = multi_file_changesets(meta, torrent)
      Repo.insert_all(__MODULE__, to_insert)
    end
    :ok
  end

  defp multi_file_changesets(meta, torrent) do
    {result, _acc} = meta.info.files
    |> Enum.map_reduce(0, fn file, acc ->
      {%{
        torrent_id: torrent.id,
        size: file.length,
        md5sum: Map.get(file, :md5sum),
        path: Path.join(file.path),
        index: acc
      }, acc + 1}
    end)
    result
  end
end
