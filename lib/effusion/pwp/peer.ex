defmodule Effusion.PWP.Peer do
  use GenServer, restart: :temporary
  alias Effusion.PWP.Messages.Handshake
  alias Effusion.PWP.Messages
  require Logger

  # @transport Application.get_env(:effusion, :peer_transport)

  ## API

  def connect({host, port}, peer_id, info_hash) do
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

    :ok = :gen_tcp.send(socket, Handshake.encode(state.peer_id, state.info_hash))
    {:ok, handshake} = :gen_tcp.recv(socket, 68)
    {:ok, _} = Effusion.PWP.Messages.Handshake.decode(IO.iodata_to_binary(handshake))

    :inet.setopts(socket, active: true, packet: 4)

    # {:ok, request} = Messages.encode({:request, 973, 0, 32768})
    # Logger.info("about to send this message: #{inspect(request)}")

    # :ok = :gen_tcp.send(socket, request)
    # get_message(socket)

    {:noreply, state}
  end

  def handle_info({:tcp, socket, data}, state) do
    data1 = IO.iodata_to_binary(data)
    {:ok, msg1} = Messages.decode(data1)
    Logger.info "Got message #{inspect(msg1)}"
    {:noreply, state}
  end

  def handle_info({:tcp_closed, socket}, state) do
    Logger.info("TCP closed connection to #{inspect(state.host)}:#{state.port}")
    {:stop, :normal, state}
  end
end
