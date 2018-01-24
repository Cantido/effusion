defmodule Effusion.PWP.Peer do
  use GenServer, restart: :temporary
  alias Effusion.PWP.Messages.Handshake

  @transport Application.get_env(:effusion, :peer_transport)

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
    {:ok, socket} = connect(state)

    state1 = Map.put(state, :socket, socket)

    {:ok, hs} = handshake(state1)
    {:ok, :unchoke} = @transport.recv(socket, 0)
    {:noreply, state1}
  end

  defp connect(state) do
    @transport.connect(state.host, state.port, [])
  end

  defp handshake(state) do
    :ok = @transport.send(state.socket, Handshake.encode(state.peer_id, state.info_hash))
    {:ok, handshake} = @transport.recv(state.socket, 68)
    Effusion.PWP.Messages.Handshake.decode(IO.iodata_to_binary(handshake))
  end
end
