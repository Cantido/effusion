alias Effusion.Session

defmodule Effusion do
  @moduledoc """
  A BitTorrent library.
  """
  @typep hash :: <<_::20, _::_*8>>
  @type info_hash :: hash()
  @type peer_id :: hash()

  @peer_client Application.get_env(:effusion, :pwp_client)

  def add_torrent(file, peer_id, ip, port) when is_binary(file) do
    {:ok, meta} = Effusion.Metainfo.decode(file)
    session_opts = [ file, peer_id, ip, port ]
    {:ok, session} = Session.start_link(session_opts)
    :ok = Session.announce(session)

    {:ok, peer} = Session.select_peer(session)
    {:ok, pid} = @peer_client.connect(peer.ip, peer.port, peer_id, meta.info_hash)
    {:ok, :unchoke} = @peer_client.recv(pid)

    {:ok, session}
  end
end
