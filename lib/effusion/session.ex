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

  def handle_call({:block, block}, _from, state) do
    Logger.info("handling call to add block #{inspect(block)}")
    state1 = Map.update(state, :blocks, [block], &([block | &1]))

    case get_ready_pieces(state1) do
      [] -> {:reply, :ok, state1}
      [piece] ->
        write_piece(piece, state1)
    end
  end

  defp get_ready_pieces(state) do
    info = state.meta.info
    piece_length = info.piece_length

    last_piece_index = div(info.length, info.piece_length)
    last_piece_length = rem(info.length, info.piece_length)

    state.blocks
    |> Enum.chunk_by(fn b -> b.index end)
    |> Enum.filter(fn(b) ->
        i = hd(b).index
        blen = block_length(b)
        if i == last_piece_index do
          blen == last_piece_length
        else
          blen == piece_length
        end
      end)
    |> Enum.map(&blocks_to_piece/1)
  end

  defp blocks_to_piece(blocks) when is_list(blocks) do
    blocks
    |> Enum.sort_by(&(&1[:offset]))
    |> Enum.reduce(fn(b, acc) ->
         %{
           index: b.index,
           data: acc.data <> b.data
         }
       end)
  end

  defp block_length(blocks) when is_list(blocks) do
    blocks
    |> Enum.map(fn b -> byte_size(b.data) end)
    |> Enum.sum()
  end

  defp write_piece(%{index: i, data: d} = piece, state) when is_binary(d) do
    Logger.debug("Checking and writing piece #{inspect(piece)}")
    expected_hash = state.meta.info.pieces |> Enum.at(i)
    actual_hash = :crypto.hash(:sha, d)

    if expected_hash == actual_hash do
      file = state.file
      piece_start_byte = i * state.meta.info.piece_length
      _ = Logger.debug("writing #{inspect(d)} to #{inspect(file)}")
      :ok = :file.pwrite(file, {:bof, piece_start_byte}, [d])
      _ = Logger.info("We successfully verified a piece!!!")
      state = Map.update!(state, :blocks, &Enum.reject(&1, fn p -> p.index == i end))
      {:reply, :ok, state}
    else
      _ = Logger.warn("Error while verifying piece #{inspect(piece)}, expected hash #{Base.encode16(expected_hash, case: :lower)} but got #{Base.encode16(actual_hash, case: :lower)}")
      {:reply, :ok, state}
    end
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
