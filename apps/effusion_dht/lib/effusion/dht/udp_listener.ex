defmodule Effusion.DHT.UDPListener do
  use GenServer
  alias Effusion.DHT.KRPC
  alias Effusion.DHT.Server

  require Logger

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  def init(opts) do
    state = %{
      port: Keyword.fetch!(opts, :port),
      server: Server.new(Keyword.fetch!(opts, :node_id))
    }

    {:ok, state, {:continue, :open_socket}}
  end

  def handle_continue(:open_socket, state = %{port: port}) do
    {:ok, socket} = :gen_udp.open(port, [:binary, {:active, 1}])
    state = Map.put(state, :socket, socket)
    {:noreply, state}
  end

  def handle_info({:udp, socket, ip, port, packet}, state) do
    message = KRPC.decode!(packet)

    context = %{
      ip: ip,
      port: port
    }

    {reply, next_server} =
      try do
        Server.handle_message(state.server, message, context)
      rescue
        err ->
          if message["y"] == "q" do
            Logger.error("Error while handling message: #{inspect err}")
            {
              KRPC.new_error(message["t"], [202, "Server Error"]),
              state.server
            }
          else
            {:noreply, state.server}
          end
      end

    Task.start(fn ->
      reply = KRPC.encode!(reply)
      :ok = :gen_udp.send(socket, ip, port, reply)
    end)

    {:noreply, %{state | server: next_server}}
  end

  def handle_info({:udp_passive, socket}, state) do
    :ok = :inet.setopts(socket, active: 1)
    {:noreply, state}
  end
end
