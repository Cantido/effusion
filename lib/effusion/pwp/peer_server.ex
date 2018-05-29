defmodule Effusion.PWP.PeerServer do
  use GenServer, restart: :temporary
  alias Effusion.PWP.Messages
  alias Effusion.BTP.Peer
  alias Effusion.SessionServer
  require Logger
  @moduledoc """
  A connection to a peer.

  This connection sends messages back to the parent SessionServer containing
  completed blocks, expecting the parent SessionServer to keep track of them.
  It also makes the SessionServer responsible for selecting which pieces to request.
  """

  # @transport Application.get_env(:effusion, :peer_transport)

  ## API

  def connect(address, peer_id, info_hash, session) when is_binary(peer_id) and byte_size(peer_id) == 20 and is_binary(info_hash) and byte_size(info_hash) == 20 do
    args = [address, peer_id, info_hash, session]
    Effusion.PWP.ConnectionSupervisor.start_child(args)
  end

  def start_link([address, peer_id, info_hash, session]) do
    GenServer.start_link(__MODULE__, [address, peer_id, info_hash, session])
  end

  ## Callbacks

  def init([address, peer_id, info_hash, session]) do
    {
      :ok,
      Peer.new(address, peer_id, info_hash, session),
      0
    }
  end

  def handle_info(:timeout, state) do
    _ = Logger.info("in timeout block")
    with {host, port} <- Peer.address(state),
         {:ok, socket} <- :gen_tcp.connect(host, port, [active: false], 1_000),
         state <- Map.put(state, :socket, socket),
         :ok <- Logger.info("opened connection"),
         :ok <- :gen_tcp.send(socket, Peer.get_handshake(state)),
         {:ok, hs_bin} <- :gen_tcp.recv(socket, 68),
         {:ok, hs = {:handshake, _, _, _}} <- Messages.decode(IO.iodata_to_binary(hs_bin)),
         {:ok, state} <- Peer.handshake(state, hs),
         :ok <- :inet.setopts(socket, active: true, packet: 4)
    do
      {:noreply, state}
    else
      err -> {:stop, :failed_handshake, err, state}
    end
  end


  def handle_info({:tcp, _socket, data}, state) do
    with data1 <- IO.iodata_to_binary(data),
         {:ok, msg1} <- Messages.decode(data1),
         :ok <- Logger.info "Got message #{inspect(msg1)}"
    do
      state = case msg1 do
        {:bitfield, _} ->
          {state1, messages} = Peer.recv_bitfield(state)
          Enum.map(messages, fn(m) -> :ok = send_msg(m, state) end)
          state1
        :unchoke ->
          state = state
            |> Peer.recv_unchoke()
            |> request_block()
        {:piece, block} ->
          Effusion.SessionServer.block(state.session, block)
          request_block(state)
      end
      {:noreply, state}
    else
      {:error, reason} -> {:stop, reason, state}
    end
  end

  def handle_info({:tcp_closed, _socket}, state) do
    {:stop, :normal, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
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
    case SessionServer.next_request(state.session) do
      %{index: i, offset: o, size: s} ->
        :ok = send_msg({:request, i, o, s}, state)
        state
      :done -> state
    end
  end
end
