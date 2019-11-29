defmodule Effusion.BTP.Download do
  alias Effusion.BTP.PeerSelection
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Piece
  alias Effusion.BTP.Peer
  alias Effusion.BTP.PeerPiece
  alias Effusion.BTP.Request
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.Metainfo
  alias Effusion.Repo
  import Effusion.BTP.Peer, only: [is_peer_id: 1]
  import Effusion.Hash, only: [is_hash: 1]
  import Ecto.Query
  require Logger
  use Timex

  @moduledoc """
  The top-level data structure of a downloading file.

  This module decides when to connect to peers, when to ask for pieces,
  when to write pieces to disk, etc.
  """

  defstruct [
    :file,
    :meta,
    :peer_id,
    :pieces,
    :local_address,
    :info_hash,
    :announce,
    started_at: nil,
    listeners: MapSet.new(),
    trackerid: ""
  ]


  @local_peer_id Application.get_env(:effusion, :peer_id)

  @doc """
  Create a new download.

  This only creates a data structure. To actually start the download, call `start/1`.
  """
  def new(meta, local_address, file \\ nil) do
    info_hash = meta.info_hash

    torrent = Repo.one(from t in Torrent, where: t.info_hash == ^info_hash)
    if is_nil(torrent) do
      {:ok, _torrent} = Torrent.insert(meta)
    end

    %__MODULE__{
      file: file,
      info_hash: meta.info_hash,
      announce: meta.announce,
      meta: meta,
      peer_id: @local_peer_id,
      pieces: Pieces.new(meta.info_hash),
      local_address: local_address
    }
  end
end
