defmodule Effusion.DHT.UDPListener do
  use GenServer
  alias Effusion.DHT.KRPC
  alias Effusion.DHT.ServerManager

  require Logger

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  def init(opts) do
    state = %{
      port: Keyword.fetch!(opts, :port),
      server: Keyword.fetch!(opts, :server)
    }

    {:ok, state, {:continue, :open_socket}}
  end

  def handle_continue(:open_socket, state = %{port: port}) do
    {:ok, socket} = :gen_udp.open(port, [:binary, {:active, 1}])
    state = Map.put(state, :socket, socket)
    {:noreply, state}
  end

  def handle_info({:udp, socket, ip, port, packet}, state = %{server: server}) do
    message = KRPC.decode!(packet)

    context = %{
      ip: ip,
      port: port
    }

    reply =
      try do
        ServerManager.handle_message(server, message, context)
      rescue
        err ->
          if message["y"] == "q" do
            Logger.error("Error while handling message: #{inspect err}")
            KRPC.new_error(message["t"], [202, "Server Error"])
          else
            raise "I don't know how to handle responses yet :("
          end
      end

    Task.start(fn ->
      reply = KRPC.encode!(reply)
      :ok = :gen_udp.send(socket, ip, port, reply)
    end)

    {:noreply, state}
  end

  def handle_info({:udp_passive, socket}, state) do
    :ok = :inet.setopts(socket, active: 1)
    {:noreply, state}
  end

  def terminate(reason, %{socket: socket}) do
    {:ok, port} = :inet.port(socket)
    Logger.warn("UDP Socket on port #{port} closing with reason #{inspect reason}")
    :gen_udp.close(socket)
  end
end
