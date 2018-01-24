defmodule Effusion.PWP.Peer do
  @transport Application.get_env(:effusion, :peer_transport)

  @spec connect(
    host:: :inet.hostname() | :inet.ip_address(),
    port:: :inet.port_number(),
    peer_id :: Effusion.peer_id(),
    info_hash :: Effusion.info_hash()
  ) :: {:ok, pid()} | :error
  def connect(host, port, _peer_id, _info_hash) do
    @transport.connect(host, port, [])
  end

  @type message :: atom

  @spec recv(pid) :: {:ok, message} | :error
  def recv(socket) do
    @transport.recv(socket, 0)
  end
end
