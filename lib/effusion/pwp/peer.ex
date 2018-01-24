defmodule Effusion.PWP.Peer do
  use Connection

  @transport Application.get_env(:effusion, :peer_transport)

  def start_link([host, port, peer_id, info_hash]) do
    Connection.start_link(__MODULE__, [{host, port}, peer_id, info_hash])
  end

  def await(pid) do
    Connection.call(pid, :await)
  end

# [ip, port, peer_id, info_hash]
  def init([{host, port}, peer_id, info_hash]) do
    {
      :connect,
      {host, port},
      %{peer_id: peer_id, info_hash: info_hash}
    }
  end

  def connect({host, port}, state) do
    {:ok, socket} = @transport.connect(host, port, [])
    {:ok, :unchoke} = recv(socket)
    {:ok, state}
  end

  def recv(socket) do
    @transport.recv(socket, 0)
  end

  def handle_call(:await, _from, state) do
    {:reply, :ok, state}
  end
end
