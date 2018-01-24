defmodule Effusion.PWP.Peer do
  use GenServer

  @transport Application.get_env(:effusion, :peer_transport)

  def start_link([host, port, peer_id, info_hash]) do
    GenServer.start_link(__MODULE__, [host, port, peer_id, info_hash])
  end

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
    {:ok, socket} = @transport.connect(state.host, state.port, [])
    {:ok, :unchoke} = @transport.recv(socket, 0)
    {:noreply, Map.put(state, :socket, socket)}
  end
end
