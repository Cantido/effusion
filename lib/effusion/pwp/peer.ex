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
    {:ok, socket} = :gen_tcp.connect(state.host, state.port, [active: false])
    state = Map.put(state, :socket, socket)

    :ok = :gen_tcp.send(socket, Handshake.encode(state.peer_id, state.info_hash))
    {:ok, handshake} = :gen_tcp.recv(socket, 68)
    {:ok, _} = Effusion.PWP.Messages.Handshake.decode(IO.iodata_to_binary(handshake))

    :inet.setopts(socket, active: true, packet: 4)


    {:noreply, state}
  end

  defp send_msg(msg, %{socket: socket}) do
    {:ok, request} = Messages.encode(msg)
    Logger.info("Sending message: #{inspect(msg)}")
    :gen_tcp.send(socket, request)
  end

  def handle_info({:tcp, socket, data}, state) do
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
          |> Map.update(:blocks, MapSet.new, &MapSet.put(&1, block))
          |> request_block()
    end

    {:noreply, state}
  end

  defp request_block(state) do
    {i, o, s} = Map.get(state, :next_block, {0, 0, 16384})
    next_block = increment_block({i, o, s}, 1048576)
    state = Map.put(state, :next_block, next_block)
    :ok = send_msg({:request, i, o, s}, state)
    state
  end

  defp increment_block({index, offset, size}, piece_size) do
    next_offset = offset + size
    if next_offset == piece_size do
      {index + 1, 0, size}
    else
      {index, offset + size, size}
    end
  end

  # block size: 16384
  # piece size: 1048576, which means 64 pieces of size 16384
  # first piece hash: 167, 53, 69, 58, 13, 103, 134, 251, 174, 104, 105, 210, 94, 112, 197, 52, 205, 246, 155, 130

  def handle_info({:tcp_closed, socket}, state) do
    Logger.info("TCP closed connection to #{inspect(state.host)}:#{state.port}")
    {:stop, :normal, state}
  end
end
