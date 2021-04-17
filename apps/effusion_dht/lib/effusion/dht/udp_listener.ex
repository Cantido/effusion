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

  def handle_continue(:open_socket, state = %{port: port}) do
    {:ok, socket} = :gen_udp.open(port, [:binary, {:active, 1}])
    state =
      state
      |> Map.put(:port, port)
      |> Map.put(:socket, socket)
    {:noreply, state}
  end

  def handle_info({:udp, socket, ip, port, packet}, state) do
    message = KRPC.decode!(packet)

    state =
      case message["q"] do
        "ping" ->

          response_params = %{
            sender_id: DHT.local_node_id(),
          }

          response =
            message["t"]
            |> KRPC.new_response(response_params)
            |> KRPC.encode!()

          peer = %{
            id: message["a"]["sender_id"],
            ip: ip,
            port: port
          }
          :ok = :gen_udp.send(socket, ip, port, response)

          Map.update(state, :peers, [peer], &([peer | &1]))
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
          state
        "find_node" ->
          nodes =
            Map.get(state, :peers, [])
            |> Enum.map(fn peer ->
              {ip0, ip1, ip2, ip3} = peer.ip
              peer.id <> <<ip0, ip1, ip2, ip3>> <> <<peer.port::integer-size(16)>>
            end)
            |> Enum.join()

          response_params = %{
            sender_id: DHT.local_node_id(),
            nodes: nodes
          }

          response =
            message["t"]
            |> KRPC.new_response(response_params)
            |> KRPC.encode!()

          :ok = :gen_udp.send(socket, ip, port, response)
          state
        "announce_peer" ->
          response =
            message["t"]
            |> KRPC.new_error([203, "Bad token"])
            |> KRPC.encode!()

          :ok = :gen_udp.send(socket, ip, port, response)
          state
        _ ->
          Logger.error("Unrecognized message: #{inspect message}")
          state
      end

    {:noreply, state}
  end

  def handle_info({:udp_passive, socket}, state) do
    :ok = :inet.setopts(socket, active: 1)
    {:noreply, state}
  end
end
