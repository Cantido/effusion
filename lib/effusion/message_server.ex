require Logger

defmodule Effusion.MessageServer do
  defp handshake do
    name_size = <<19>>
    name = "BitTorrent protocol"
    reserved = <<0 :: size(64)>>
    info_hash = <<0 :: size(160)>>
    peer_id = <<0 :: size(160)>>

    name_size <> name <> reserved <> info_hash <> peer_id
  end

  def accept(port) do
    socket_opts =
      [:binary,
       packet: 0, # This should be changed to 4 after we accept a handshake
       active: false,
       reuseaddr: true]
    {:ok, socket} = :gen_tcp.listen(port, socket_opts)
    Logger.info "Accepting connections on port #{port}"
    loop_acceptor(socket)
  end

  defp loop_acceptor(socket) do
    {:ok, client} = :gen_tcp.accept(socket)
    {:ok, pid} = Task.Supervisor.start_child(Effusion.TaskSupervisor, fn -> serve(client) end)
    :ok = :gen_tcp.controlling_process(client, pid)
    loop_acceptor(socket)
  end

  defp serve(socket) do
    {:ok, data} = :gen_tcp.recv(socket, 0)

    <<19, "BitTorrent protocol",
    _reserved :: size(64),
    info_hash :: size(160),
    peer_id :: size(160)>> = data

    Logger.info ("Handshake from peer_id #{inspect(peer_id)} for info_hash #{inspect(info_hash)}")

    :gen_tcp.send(socket, handshake())
    :gen_tcp.shutdown(socket, :read_write)
  end
end
