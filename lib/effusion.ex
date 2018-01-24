alias Effusion.Session

defmodule Effusion do
  @moduledoc """
  A BitTorrent library.
  """
  @typep hash :: <<_::20, _::_*8>>
  @type info_hash :: hash()
  @type peer_id :: hash()

  def add_torrent(file, peer_id, ip, port) when is_binary(file) do
    session_opts = [ file, peer_id, ip, port ]
    Session.start_link(session_opts)
  end
end
