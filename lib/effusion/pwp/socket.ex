defmodule Effusion.PWP.Socket do
  require Logger
  alias Effusion.PWP.Messages
  alias Effusion.BTP.Peer

  def listen(port) do
    :gen_tcp.listen(port, [:binary, active: false, reuseaddr: true, send_timeout: 1_000])
  end

  def connect(peer) do
    with {host, port} = peer.address,
         {:ok, socket} <- :gen_tcp.connect(host, port, [active: false], 1_000),
         :ok <- send_msg(socket, Peer.get_handshake(peer)),
         {:ok, hs = {:handshake, _, _, _}} <- recv(socket, 68),
         {:ok, peer} <- Peer.handshake(peer, hs),
         :ok <- :inet.setopts(socket, packet: 4)
    do
      {:ok, socket, peer}
    else
      err -> err
    end
  end

  def accept(lsock, peer) do
    with {:ok, socket} <- :gen_tcp.accept(lsock, 1_000),
         {:ok, hs = {:handshake, _, _, _}} <- recv(socket, 68),
         {:ok, peer} <- Peer.handshake(peer, hs),
         :ok <- send_msg(socket, Peer.get_handshake(peer)),
         :ok <- :inet.setopts(socket, packet: 4)
    do
      {:ok, socket, peer}
    else
      err -> err
    end
  end

  def send_all(socket, messages) do
    _ = Enum.map(messages, fn(m) -> :ok = send_msg(socket, m) end)
    :ok
  end

  def send_msg(socket, msg) do
    {:ok, request} = Messages.encode(msg)
    :gen_tcp.send(socket, request)
  end

  def recv(socket, size \\ 0) do
    {:ok, packet} = :gen_tcp.recv(socket, size)
    decode(packet)
  end

  def decode(data) do
    data1 = IO.iodata_to_binary(data)
    Messages.decode(data1)
  end

  def close(socket) do
    :gen_tcp.close(socket)
  end
end
