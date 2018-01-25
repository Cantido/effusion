defmodule Effusion.Session do
  use GenServer
  require Logger
  alias Effusion.Metainfo

  @thp_client Application.get_env(:effusion, :thp_client)

  ## API

  def start(opts) do
    Effusion.SessionSupervisor.start_child(opts)
  end

  def start_link([metabin, peer_id, local_peer]) when is_binary(metabin) do
    GenServer.start_link(__MODULE__, [metabin, peer_id, local_peer])
  end

  ## Callbacks

  def init([meta_bin, peer_id, local_peer]) do
    {:ok, meta} = Metainfo.decode(meta_bin)

    state = %{
      meta: meta,
      local_peer: local_peer,
      peer_id: peer_id
    }

    {:ok, state, 0}
  end

  def handle_info(:timeout, state) do
    state1 = do_announce(state)
    peer = do_select_peer(state1)

    {:ok, _socket} = Effusion.PWP.Peer.connect({peer.ip, peer.port}, state1.peer_id, state1.meta.info_hash)
    {:noreply, state1}
  end

  defp do_announce(state) do
    {local_host, local_port} = state.local_peer

    Logger.info("Announcing torrent #{Base.encode16 state.meta.info_hash, case: :lower} to #{inspect(state.meta.announce)} that I'm listening at #{inspect(state.local_peer)}")
    {:ok, res} = @thp_client.announce(
      state.meta.announce,
      local_host,
      local_port,
      state.peer_id,
      state.meta.info_hash,
      0,
      0,
      state.meta.info.length
    )
    Logger.info("Announce finished, got #{length(res.peers)} peers.")

    Map.put(state, :peers, res.peers)
  end

  defp do_select_peer(state) do
    state.peers
       |> Enum.find(fn(p) -> p.peer_id != state.peer_id end)
  end
end
