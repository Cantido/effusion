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

  @doc """
  Get the blocks that have not been assembled into pieces and verified.
  """
  def blocks(d = %__MODULE__{}) do
    Pieces.unfinished(d.pieces)
  end

  @doc """
  Get the set of listeners waiting for this torrent to finish.
  """
  def listeners(d = %__MODULE__{}) do
    d.listeners
  end

  @doc """
  Get the pieces that this download is downloading.
  """
  def pieces(d = %__MODULE__{}) do
    d.pieces
  end

  def bytes_completed(download) do
    Pieces.bytes_completed(download.pieces)
  end

  def download_size(download) do
    Metainfo.bytes_count(download.meta)
  end

  def downloaded_ratio(download) do
    {
      bytes_completed(download),
      download_size(download)
    }
  end

  def download_duration(download) do
    Timex.Interval.new(from: download.started_at, until: Timex.now())
    |> Timex.Interval.duration(:duration)
  end

  @doc """
  Add a process that should be notified when this download completes or crashes.
  """
  def add_listener(d = %__MODULE__{}, from) do
    Map.update(d, :listeners, MapSet.new(), &MapSet.put(&1, from))
  end

  @doc """
  Perform a function on all of this download's listening processes.
  """
  def each_listener(%__MODULE__{listeners: listeners}, fun)
      when is_function(fun, 1) do
    Enum.each(listeners, &fun.(&1))
  end

  @doc """
  Check if this download has received all necessary bytes.
  """
  def done?(d = %__MODULE__{}) do
    Pieces.all_present?(d.pieces)
  end
end
