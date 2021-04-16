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
    {:ok, socket} = :gen_udp.open(port, [:binary, {:active, 1}])
    {:noreply, %{port: port, socket: socket}}
  end

  def handle_info({:udp, socket, ip, port, packet}, state) do
    message = KRPC.decode!(packet)

    case message["q"] do
      "ping" ->
        response =
          message["t"]
          |> KRPC.new_response(%{sender_id: DHT.local_node_id()})
          |> KRPC.encode!()

        :ok = :gen_udp.send(socket, ip, port, response)
      "get_peers" ->
        response_params = %{
          sender_id: DHT.local_node_id(),
          token: DHT.generate_announce_peer_token(),
          nodes: <<>>
        }

        response =
          message["t"]
          |> KRPC.new_response(response_params)
          |> KRPC.encode!()

        :ok = :gen_udp.send(socket, ip, port, response)
      _ ->
        Logger.error("Unrecognized message: #{inspect message}")
    end

    {:noreply, state}
  end

  def handle_info({:udp_passive, socket}, state) do
    :ok = :inet.setopts(socket, active: 1)
    {:noreply, state}
  end
end
