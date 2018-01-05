require Logger

defmodule Effusion.PWP.Server do
  @moduledoc """
  Listen for TCP connections from peers and dispatch them.
  """

  @socket_opts [:binary,
                packet: 0, # This should be changed to 4 after we accept a handshake
                packet_size: 68,
                active: false,
                reuseaddr: true]

  @doc """
  Listen on the given port for TCP connections.
  """
  @spec listen(:inet.port_number()) :: no_return()
  def listen(port) do
    {:ok, socket} = :gen_tcp.listen(port, @socket_opts)
    :ok = Logger.info "Accepting connections on port #{port}"
    loop_acceptor(socket)
  end

  defp loop_acceptor(socket) do
    {:ok, client} = :gen_tcp.accept(socket)
    {:ok, pid} = Effusion.PWP.PeerConnectionSupervisor.start_child(client)
    :ok = :gen_tcp.controlling_process(client, pid)
    loop_acceptor(socket)
  end
end
