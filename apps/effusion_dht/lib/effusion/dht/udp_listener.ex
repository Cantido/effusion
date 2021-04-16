defmodule Effusion.DHT.UDPListener do
  use GenServer
  alias Effusion.DHT
  alias Effusion.DHT.KRPC

  require Logger

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  def init(opts) do
    port = Keyword.fetch!(opts, :port)
    {:ok, %{port: port}, {:continue, :open_socket}}
  end

  def handle_continue(:open_socket, %{port: port}) do
    socket = :gen_udp.open(port, [:binary, {:active, :once}])
    {:noreply, %{port: port, socket: socket}}
  end

  def handle_info({:udp, _socket, ip, port, packet}, state) do
    message = KRPC.decode!(packet)

    if message["q"] == "ping" do
      :ok =
        message["t"]
        |> KRPC.new_response(%{sender_id: DHT.local_node_id()})
        |> KRPC.encode!()
        |> KRPC.send_message(ip, port)
    else
      Logger.error("Unrecognized message: #{message}")
    end

    {:noreply, state}
  end

  def handle_info({:udp_passive, socket}, state) do
    :ok = :inet.setopts(socket, active: :once)
    {:noreply, state}
  end
end
