defmodule Effusion.Session do
  use GenServer
  require Logger

  @thp_client Application.get_env(:effusion, :thp_client)
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

  ## Callbacks

  def init([meta, local_peer, file]) do
    state = %{
      file: file,
      meta: meta,
      local_peer: local_peer,
      peer_id: @local_peer_id
    }

    {:ok, state, 0}
  end

  def handle_call({:block, %{index: i, data: d}}, _from, state) when is_binary(d) do
    with expected_hash <- state.meta.info.pieces |> Enum.at(i),
         ^expected_hash <- :crypto.hash(:sha, d),
         :ok <- IO.binwrite(state.file, d),
         _ = Logger.info("We successfully verified a piece!!!")
    do
      {:reply, :ok, state}
    else
      err ->
        _ = Logger.warn("Error while verifying piece: #{inspect(err)}")
        {:reply, :ok, state}
    end
  end

  def handle_info(:timeout, state) do
    state1 = do_announce(state)
    case do_select_peer(state1) do
      nil ->
        {:noreply, state1}
      peer ->
        {:ok, _socket} = Effusion.PWP.Peer.connect({peer.ip, peer.port}, state1.peer_id, state1.meta.info_hash)
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
