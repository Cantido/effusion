defmodule Effusion.PWP.Protocol do
  @behaviour :ranch_protocol

  def start_link(listener, socket, transport, opts) do
    pid = spawn_link(fn -> init(listener, socket, transport, opts) end)
    {:ok, pid}
  end

  def init(listener, socket, transport, _opts = []) do
    :ok = :ranch.accept_ack(listener)
    loop(socket, transport)
  end

  def loop(socket, transport) do
    case transport.recv(socket, 0, 5000) do
      {:ok, data} ->
        transport.send(socket, data)
        loop(Socket, Transport)
      _ ->
        :ok = transport.close(socket)
    end
  end
end
