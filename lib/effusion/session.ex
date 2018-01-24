defmodule Effusion.Session do
  use GenServer
  alias Effusion.Metainfo

  @thp_client Application.get_env(:effusion, :thp_client)

  ## API

  def start_link([metabin, peer_id, ip, port]) when is_binary(metabin) do
    GenServer.start_link(__MODULE__, [metabin, peer_id, ip, port])
  end

  def announce(pid) do
    GenServer.call(pid, :announce)
  end

  def select_peer(pid) do
    GenServer.call(pid, :select_peer)
  end

  def select_peer(peers, peer_id) do
    IO.puts "Peers to choose from: #{inspect(peers, pretty: true)}"
    peers |> Enum.find(fn(p) -> p["peer_id"] != peer_id end)
  end

  ## Callbacks

  def init([meta_bin, peer_id, ip, port]) do
    {:ok, meta} = Metainfo.decode(meta_bin)

    state = %{
      meta: meta,
      ip: ip,
      port: port,
      peer_id: peer_id
    }

    {:ok, state, 0}
  end

  def handle_call(:announce, _from, state) do
    state1 = do_announce(state)

    {:reply, :ok, state1}
  end

  def handle_call(:select_peer, _from, state) do
    peer = do_select_peer(state)

    {:reply, {:ok, peer}, state}
  end

  def handle_info(:timeout, state) do
    state1 = do_announce(state)
    peer = do_select_peer(state1)

    {:ok, _socket} = Effusion.PWP.Peer.connect({peer.ip, peer.port}, state1.peer_id, state1.meta.info_hash)
    {:noreply, state1}
  end

  defp do_announce(state) do
    {:ok, res} = @thp_client.announce(
      state.meta.announce,
      state.ip,
      state.port,
      state.peer_id,
      state.meta.info_hash,
      0,
      0,
      state.meta.info.length
    )

    Map.put(state, :peers, res.peers)
  end

  defp do_select_peer(state) do
    state.peers
       |> Enum.find(fn(p) -> p["peer_id"] != state.peer_id end)
  end
end
