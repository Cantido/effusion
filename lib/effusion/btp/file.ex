defmodule Effusion.BTP.File do
  alias Effusion.BTP.Torrent
  alias Effusion.Range
  alias Effusion.Repo
  import Effusion.Hash
  import Ecto.Changeset
  import Ecto.Query
  use Ecto.Schema

  @moduledoc """
  A downloadable file within a torrent.

  Some torrents describe only a single file, while others describe many.
  """

  schema "files" do
    belongs_to :torrent, Torrent
    field :size, :integer, null: false
    field :path, :string, null: false
    field :index, :integer, null: false
    field :md5sum, :binary, null: true
  end

  def changeset(file, params \\ %{}) do
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

  def get(info_hash) when is_hash(info_hash) do
    from file in __MODULE__,
    join: torrent in assoc(file, :torrent),
    where: torrent.info_hash == ^info_hash
  end
end
