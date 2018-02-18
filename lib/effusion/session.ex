defmodule Effusion.Session do
  use GenServer
  require Logger
  alias Effusion.BTP.Torrent

  @thp_client Application.get_env(:effusion, :thp_client)
  @block_size Application.get_env(:effusion, :block_size)
  @local_peer_id "Effusion Experiment!"

  ## API

  def start(meta, {_host, _port} = local_server, file \\ nil) when is_map(meta) do
    Effusion.SessionSupervisor.start_child([meta, local_server, file])
  end

  def start_link([meta, local_peer]) do
    start_link([meta, local_peer, nil])
  end

  def start_link([meta, local_peer, file]) do
    GenServer.start_link(__MODULE__, [meta, local_peer, file])
  end

  def block(pid, block) do
    GenServer.call(pid, {:block, block})
  end

  def blocks(pid) do
    GenServer.call(pid, :get_blocks)
  end

  def next_request(pid) do
    GenServer.call(pid, :next_request)
  end

  ## Callbacks

  defguard is_index(i) when is_integer(i) and i >= 0
  defguard is_size(x) when is_integer (x) and x > 0

  def init([meta, local_peer, file]) do
    state = %{
      file: file,
      meta: meta,
      torrent: Torrent.new(meta.info),
      local_peer: local_peer,
      peer_id: @local_peer_id
    }

    {:ok, state, 0}
  end

  def handle_call(:get_blocks, _from, state) do
    {:reply, Torrent.blocks(state.torrent), state}
  end

  def handle_call({:block, block}, _from, state) do
    {:ok, torrent1} = state.torrent
    |> Torrent.add_block(block)
    |> Torrent.write_to(state.file)

    {:reply, :ok, %{state | torrent: torrent1}}
  end

  def handle_call(:next_request, _from, state) do
    have_pieces = Torrent.finished_pieces(state.torrent)
    next_block = Effusion.BTP.PieceSelection.next_block(state.meta.info, have_pieces, @block_size)

    {:reply, next_block, state}
  end

  def handle_info(:timeout, state) do
    state1 = do_announce(state)
    case do_select_peer(state1) do
      nil ->
        {:noreply, state1}
      peer ->
        {:ok, _socket} = Effusion.PWP.Peer.connect({peer.ip, peer.port}, state1.peer_id, state1.meta.info_hash, self())
        {:noreply, state1}
    end
  end

  defp do_announce(state) do
    {local_host, local_port} = state.local_peer

    _ = Logger.info("Announcing torrent #{Base.encode16 state.meta.info_hash, case: :lower} to #{inspect(state.meta.announce)} that I'm listening at #{inspect(state.local_peer)}")

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

    _ = Logger.info("Announce finished, got #{length(res.peers)} peers.")
    Map.put(state, :peers, res.peers)
  end

  defp do_select_peer(%{peers: []}) do
    nil
  end

  defp do_select_peer(state) do
    state.peers
       |> Enum.find(fn(p) -> p.peer_id != state.peer_id end)
  end
end
