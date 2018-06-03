defmodule Effusion.PWP.Socket do
  require Logger
  alias Effusion.PWP.Messages
  alias Effusion.BTP.Peer
  def connect(peer) do
    {host, port} = peer.address
    {:ok, socket} = :gen_tcp.connect(host, port, [active: false], 1_000)

    :ok = send_msg(socket, Peer.get_handshake(peer))
    {:ok, hs_bin} = :gen_tcp.recv(socket, 68)
    {:ok, hs = {:handshake, _, _, _}} = Messages.decode(IO.iodata_to_binary(hs_bin))
    {:ok, peer} = Peer.handshake(peer, hs)
    :ok = :inet.setopts(socket, active: true, packet: 4)
    {:ok, socket, peer}
  end

  def send_msg(socket, msg) do
    {:ok, request} = Messages.encode(msg)
    :gen_tcp.send(socket, request)
  end

  def decode(data) do
    data1 = IO.iodata_to_binary(data)
    Messages.decode(data1)
  end
end
