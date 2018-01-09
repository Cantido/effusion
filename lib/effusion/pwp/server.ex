require Logger

defmodule Effusion.PWP.Server do
  @moduledoc """
  Listen for TCP connections from peers and dispatch them.
  """

  def child_spec(_) do
    Supervisor.child_spec({Task, fn -> Effusion.PWP.Server.listen(4040) end}, restart: :permanent)
  end

  @socket_opts [:binary, reuseaddr: true]

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
    with {:ok, client} <- :gen_tcp.accept(socket),
         {:ok, pid} <- Effusion.PWP.Connection.Supervisor.start_child(client),
         :ok <- :gen_tcp.controlling_process(client, pid)
    do
      loop_acceptor(socket)
    else
      err -> err
    end
  end
end
