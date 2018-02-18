defmodule Effusion.Session do
  use GenServer
  require Logger

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
      local_peer: local_peer,
      peer_id: @local_peer_id
    }

    {:ok, state, 0}
  end

  def handle_call({:block, %{index: i, offset: o, data: d}}, _from, state) do
    block = block_data(i, o, d)
    Logger.info("handling call to add block #{inspect(block)}")
    state1 = Map.update(state, :blocks, MapSet.new([block]), &MapSet.put(&1, block))

    case get_ready_pieces(state1) do
      [] -> {:reply, :ok, state1}
      [piece] -> write_piece(piece, state1)
    end
  end

  def handle_call(:next_request, _from, state) do
    {:reply, next_block(state), state}
  end

  defp next_block(state) do
    all_blocks = all_possible_blocks(state.meta.info)
    have_pieces = Map.get(state, :have, IntSet.new())

    all_blocks
      |> Enum.reject(fn(b) -> Map.get(b, :index) in have_pieces end)
      |> hd()
  end

  defp all_possible_blocks(%{length: file_size, piece_length: whole_piece_size})  do
    file_size
    |> file_to_pieces(whole_piece_size)
    |> Enum.flat_map(&piece_to_blocks(&1, @block_size))
  end

  defp file_to_pieces(total_size, piece_size) when is_size(total_size) and is_size(piece_size) do
    {whole_piece_count, last_piece_size} = divrem(total_size, piece_size)
    whole_piece_indices = 0..(whole_piece_count - 1)

    whole_pieces =
      for i <- whole_piece_indices,
          into: MapSet.new()
      do
        %{index: i, size: piece_size}
      end

    if last_piece_size == 0 do
      whole_pieces
    else
      last_piece_index = whole_piece_count
      last_piece = %{index: last_piece_index, size: last_piece_size}
      MapSet.put(whole_pieces, last_piece)
    end
  end

  defp piece_to_blocks(%{index: index, size: piece_size} = piece, block_size) when is_index(index) and is_size(piece_size) do
    Logger.info "breaking piece #{inspect(piece)} into blocks of size #{block_size}"

    {whole_block_count, last_block_size} = divrem(piece.size, block_size)
    whole_block_indices = 0..(whole_block_count - 1)

    Logger.info "We will have #{whole_block_count} whole blocks, with a last block of size #{last_block_size}"

    whole_blocks =
      if whole_block_count > 0 do
        for b_i <- whole_block_indices,
            offset = b_i * block_size,
            into: MapSet.new()
        do
          block_id(piece.index, offset, block_size)
        end
      else
        MapSet.new()
      end
    Logger.info "got whole blocks: #{inspect(whole_blocks)}"

    if last_block_size == 0 do
      whole_blocks
    else
      last_block_index = whole_block_count
      last_block_offset = last_block_index * block_size
      last_block = block_id(piece.index, last_block_offset, last_block_size)
      MapSet.put(whole_blocks, last_block)
    end
  end

  defp divrem(a, b) do
    {div(a, b), rem(a, b)}
  end

  defp get_ready_pieces(state) do
    info = state.meta.info
    piece_length = info.piece_length

    last_piece_index = div(info.length, info.piece_length)
    last_piece_length = rem(info.length, info.piece_length)

    state.blocks
    |> Enum.chunk_by(fn b -> Map.get(b, :index) end)
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
      state = state
        |> Map.update!(:blocks, &remove_blocks_with_index(&1, i))
        |> Map.update(:have, IntSet.new(i), &IntSet.put(&1, i))
      {:reply, :ok, state}
    else
      _ = Logger.warn("Error while verifying piece #{inspect(piece)}, expected hash #{Base.encode16(expected_hash, case: :lower)} but got #{Base.encode16(actual_hash, case: :lower)}")
      {:reply, :ok, state}
    end
  end

  defp remove_blocks_with_index(blocks, i) when is_index(i) do
    blocks
    |> Enum.reject(fn p -> p.index == i end)
    |> MapSet.new()
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


  defp block_id(i, o, s) when is_index(i) and is_index(o) and is_size(s) do
    %{index: i, offset: o, size: s}
  end

  defp block_data(i, o, d) when is_index(i) and is_index(o) and is_binary(d) do
    %{index: i, offset: o, data: d}
  end
end
