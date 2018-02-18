alias Effusion.Session

defmodule Effusion do
  alias Effusion.BTP.Metainfo
  @moduledoc """
  A BitTorrent library.
  """
  @typep hash :: <<_::20, _::_*8>>
  @type info_hash :: hash()
  @type peer_id :: hash()

  def start_download(filename) when is_binary(filename) do
    local_server_address = Application.get_env(:effusion, :server_address)

    {:ok, metabin} = File.read filename
    {:ok, meta} = Metainfo.decode(metabin)
    Session.start(meta, local_server_address)
  end
end
