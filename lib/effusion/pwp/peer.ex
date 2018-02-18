defmodule Effusion.PWP.Peer do
  use GenServer, restart: :temporary
  alias Effusion.PWP.Messages.Handshake
  alias Effusion.PWP.Messages
  alias Effusion.Session
  require Logger

  # @transport Application.get_env(:effusion, :peer_transport)

  ## API

  def connect({host, port}, peer_id, info_hash, session) when is_binary(peer_id) and byte_size(peer_id) == 20 and is_binary(info_hash) and byte_size(info_hash) == 20 do
    args = [host, port, peer_id, info_hash, session]
    Effusion.PWP.ConnectionSupervisor.start_child(args)
  end

  def start_link([host, port, peer_id, info_hash, session]) do
    GenServer.start_link(__MODULE__, [host, port, peer_id, info_hash, session])
  end

  ## Callbacks

  def init([host, port, peer_id, info_hash, session]) do
    {
      :ok,
      %{
        host: host,
        port: port,
        peer_id: peer_id,
        info_hash: info_hash,
        session: session
      },
      0
    }
  end

  def handle_info(:timeout, state) do
    _ = Logger.info("in timeout block")
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
    with data1 <- IO.iodata_to_binary(data),
         {:ok, msg1} <- Messages.decode(data1),
         :ok <- Logger.info "Got message #{inspect(msg1)}"
    do
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
          parent = state.session
          Effusion.Session.block(parent, block)
          state
            |> Map.update(:blocks, MapSet.new([block]), &MapSet.put(&1, block))
            |> request_block()
      end
      {:noreply, state}
    else
      {:error, reason} -> {:stop, reason, state}
    end
  end

  def handle_info({:tcp_closed, _socket}, state) do
    {:stop, :normal, state}
  end

  def terminate(_, %{socket: socket}) do
    :gen_tcp.close(socket)
  end

  def terminate(_, _), do: :ok

  defp send_msg(msg, %{socket: socket}) do
    {:ok, request} = Messages.encode(msg)
    _ = Logger.info("Sending message: #{inspect(msg)}")
    :gen_tcp.send(socket, request)
  end

  defp request_block(state) do
    case Session.next_request(state.session) do
      %{index: i, offset: o, size: s} ->
        :ok = send_msg({:request, i, o, s}, state)
        state
      :done -> state
    end
  end
end
