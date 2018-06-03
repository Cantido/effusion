alias Effusion.Application.SessionServer

defmodule Effusion do
  alias Effusion.BTP.Metainfo
  @moduledoc """
  A BitTorrent library.
  """
  @typep hash :: <<_::20, _::_*8>>
  @type info_hash :: hash()
  @type peer_id :: hash()

  def start_download(filename, destfile) when is_binary(filename) do
    local_server_address = Application.get_env(:effusion, :server_address)

    {:ok, metabin} = File.read filename
    {:ok, meta} = Metainfo.decode(metabin)
    SessionServer.start(meta, local_server_address, destfile)
  end

  def download(filename, destfile) do
    {:ok, pid} = start_download(filename, destfile)
    SessionServer.await(pid)
  end
end
