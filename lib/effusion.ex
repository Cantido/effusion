alias Effusion.BTP.SessionServer

defmodule Effusion do
  @moduledoc """
  A BitTorrent library.
  """
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

    SessionServer.start(meta, local_server_address, destfile)
  end

  @doc """
  Start downloading the torrent described by `meta`,
  and write it to a file in the `destfile` directory.

  This function blocks until the download completes.
  """
  def download(meta, destfile) do
    case start_download(meta, destfile) do
      {:ok, pid} -> SessionServer.await(pid)
      err -> err
    end
  end
end
