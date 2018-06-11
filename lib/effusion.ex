alias Effusion.BTP.SessionServer

defmodule Effusion do
  @moduledoc """
  A BitTorrent library.
  """
  @typep hash :: <<_::20, _::_*8>>
  @type info_hash :: hash()
  @type peer_id :: hash()

  def start_download(meta, destfile) do
    local_server_address = Application.get_env(:effusion, :server_address)

    SessionServer.start(meta, local_server_address, destfile)
  end

  def download(meta, destfile) do
    {:ok, pid} = start_download(meta, destfile)
    SessionServer.await(pid)
  end
end
