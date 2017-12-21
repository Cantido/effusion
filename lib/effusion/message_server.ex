require Logger

defmodule Effusion.MessageServer do
  def listen(port) do
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
    {:ok, pid} = Task.Supervisor.start_child(Effusion.TaskSupervisor, fn -> Effusion.PeerConnection.serve(client) end)
    :ok = :gen_tcp.controlling_process(client, pid)
    loop_acceptor(socket)
  end
end
