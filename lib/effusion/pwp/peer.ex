defmodule Effusion.PWP.Peer do
  use GenServer, restart: :temporary
  alias Effusion.PWP.Messages.Handshake
  alias Effusion.PWP.Messages
  require Logger

  # @transport Application.get_env(:effusion, :peer_transport)

  ## API

  def connect({host, port}, peer_id, info_hash) when is_binary(peer_id) and byte_size(peer_id) == 20 and is_binary(info_hash) and byte_size(info_hash) == 20 do
    args = [host, port, peer_id, info_hash]
    Effusion.PWP.ConnectionSupervisor.start_child(args)
  end

  def start_link([host, port, peer_id, info_hash]) do
    GenServer.start_link(__MODULE__, [host, port, peer_id, info_hash])
  end

  ## Callbacks

  def init([host, port, peer_id, info_hash]) do
    {
      :ok,
      %{
        host: host,
        port: port,
        peer_id: peer_id,
        info_hash: info_hash
      },
      0
    }
  end

  def handle_info(:timeout, state) do
    Logger.info("in timeout block")
    with {:ok, socket} <- :gen_tcp.connect(state.host, state.port, [active: false], 1_000),
         state <- Map.put(state, :socket, socket),
         :ok <- Logger.info("opened connection"),
         :ok <- :gen_tcp.send(socket, Handshake.encode(state.peer_id, state.info_hash)),
         {:ok, handshake} <- :gen_tcp.recv(socket, 68),
         {:ok, _} <- Effusion.PWP.Messages.Handshake.decode(IO.iodata_to_binary(handshake)),
         :ok <- :inet.setopts(socket, active: true, packet: 4)
    do
      {:noreply, state}
    else
      _ -> {:stop, :failed_handshake, state}
    end
  end


  def handle_info({:tcp, _socket, data}, state) do
    data1 = IO.iodata_to_binary(data)
    {:ok, msg1} = Messages.decode(data1)
    Logger.info "Got message #{inspect(msg1)}"

    state = case msg1 do
      {:bitfield, _} ->
        :ok = send_msg(:interested, state)
        :ok = send_msg(:unchoke, state)
        state
          |> Map.put(:am_choking, false)
          |> Map.put(:am_interested, true)
      :unchoke ->
        state
          |> Map.put(:peer_choking, false)
          |> request_block()
      {:piece, block} ->
        state
          |> Map.update(:blocks, MapSet.new([block]), &MapSet.put(&1, block))
          |> verify_pieces()
          |> request_block()
    end

    {:noreply, state}
  end


  # block size: 16384
  # piece size: 1048576, which means 64 pieces of size 16384
  # first piece hash: 167, 53, 69, 58, 13, 103, 134, 251, 174, 104, 105, 210, 94, 112, 197, 52, 205, 246, 155, 130

  def handle_info({:tcp_closed, _socket}, state) do
    Logger.info("TCP closed connection to #{inspect(state.host)}:#{state.port}")
    {:stop, :normal, state}
  end

  def terminate(:normal, %{socket: socket}) do
    :gen_tcp.close(socket)
  end

  def terminate(_, _), do: :ok

  defp send_msg(msg, %{socket: socket}) do
    {:ok, request} = Messages.encode(msg)
    Logger.info("Sending message: #{inspect(msg)}")
    :gen_tcp.send(socket, request)
  end

  defp verify_pieces(state) do
    first_piece_blocks = state.blocks |> Enum.filter(&match?(%{index: 0}, &1))
    block_count = Enum.count(first_piece_blocks)
    Logger.info("We have #{block_count} blocks")
    if (block_count == 64) do
      bin = into_binary(first_piece_blocks)
      hash = :crypto.hash(:sha, bin)
      ^hash = <<167, 53, 69, 58, 13, 103, 134, 251, 174, 104, 105, 210, 94, 112, 197, 52, 205, 246, 155, 130>>
      Logger.info("We successfully verified a piece!!!")
    end
    state
  end

  defp into_binary(blocks) when is_list(blocks) do
    Enum.reduce(blocks, <<>>, &into_binary/2)
  end

  defp into_binary(%{index: i, offset: o, data: d}, bin)
       when byte_size(bin) < ((i * 16384) + o + byte_size(d)) do
    size_before_block = i * 16384 + o
    bytes_remaining = size_before_block - byte_size(bin)
    bits_remaining = bytes_remaining * 8

    if bytes_remaining > 0 do
      <<bin::binary, 0::size(bits_remaining), d::binary>>
    else
      <<bin::binary, d::binary>>
    end
  end

  defp into_binary(%{index: i, offset: o, data: d}, bin)
       when byte_size(bin) >= ((i * 16384) + o + byte_size(d)) do
    size_before_block = i * 16384 + o
    block_size = byte_size(d)
    <<
      before_block::bytes-size(size_before_block),
      _::bytes-size(block_size),
      after_block :: binary
    >> = bin
    <<before_block::binary, d::binary, after_block::binary>>
  end

  defp request_block(state) do
    {i, o, s} = Map.get(state, :next_block, {0, 0, 16384})
    if i == 0 do
      :ok = send_msg({:request, i, o, s}, state)

      next_block = increment_block({i, o, s}, 1048576)
      state = Map.put(state, :next_block, next_block)
      state
    else
      Logger.info("We are done getting blocks, next piece is #{inspect({i, o, s})}")
      state
    end
  end

  defp increment_block({index, offset, size}, piece_size) do
    next_offset = offset + size
    if next_offset == piece_size do
      {index + 1, 0, size}
    else
      {index, offset + size, size}
    end
  end
end
