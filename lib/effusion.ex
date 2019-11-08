defmodule Effusion do
  alias Effusion.BTP.DownloadServer
  alias Effusion.BTP.Metainfo

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
  def start_download(meta, destfile) do
    local_server_address = Application.get_env(:effusion, :server_address)

    DownloadServer.start(meta, local_server_address, destfile)
  end

  @doc """
  Start downloading the torrent described by `meta`,
  and write it to a file in the `destfile` directory.

  This function blocks until the download completes.
  """
  def download(meta, destfile) do
    case start_download(meta, destfile) do
      {:ok, _pid} -> DownloadServer.await(meta.info_hash)
      err -> err
    end
  end

  @doc """
  Start downloading the torrent with a metadata file located at `torrent_file`.
  """
  def download_file(torrent_file) do
    {:ok, metabin} = torrent_file |> Path.expand() |> File.read()
    {:ok, meta} = Metainfo.decode(metabin)
    dest = meta.info.name |> Path.expand()

    {:ok, _info_hash} = Effusion.start_download(meta, dest)
  end
end
