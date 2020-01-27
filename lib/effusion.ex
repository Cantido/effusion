defmodule Effusion do
  alias Effusion.Application.DownloadsSupervisor
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.Metainfo
  alias Effusion.BTP.ProtocolHandler, as: BTPProtocolHandler
  alias Effusion.Repo
  import Ecto.Query

  @moduledoc """
  A BitTorrent library.
  """

  # credo:disable-for-next-line
  @typep hash :: <<_::20, _::_*8>>
  @type info_hash :: hash()
  @type peer_id :: hash()

  @doc """
  Start asynchronously downloading the torrent described by `meta`,
  and write it to a file in the `destfile` directory.

  This function returns immediately.
  """
  def start_download(meta) when is_map(meta) do
    info_hash = meta.info_hash
    torrent = Repo.one(from t in Torrent, where: t.info_hash == ^info_hash)
    if is_nil(torrent) do
      {:ok, _torrent} = Torrent.insert(meta)
    end

    start_download(info_hash)
  end

  def start_download(info_hash) do
    {:ok, pid} = DownloadsSupervisor.start_child(info_hash)
    :ok = BTPProtocolHandler.start(info_hash)
    {:ok, pid}
  end

  def pause_download(pid) when is_pid(pid) do
    DynamicSupervisor.terminate_child(DownloadsSupervisor, pid)
  end

  def stop_download(pid) when is_pid(pid) do
    DynamicSupervisor.terminate_child(DownloadsSupervisor, pid)
  end

  @doc """
  Start downloading the torrent described by `meta`,
  and write it to a file in the `destfile` directory.

  This function blocks until the download completes.
  """
  def download(meta) do
    case start_download(meta) do
      {:ok, _pid} -> BTPProtocolHandler.await(meta.info_hash)
      err -> err
    end
  end

  @doc """
  Start downloading the torrent with a metadata file located at `torrent_file`.
  """
  def download_file(torrent_file) do
    {:ok, metabin} = torrent_file |> Path.expand() |> File.read()
    {:ok, meta} = Metainfo.decode(metabin)

    {:ok, _info_hash} = Effusion.start_download(meta)
  end
end
