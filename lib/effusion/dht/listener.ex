defmodule Effusion.DHT.Listener do
  use GenServer
  alias Effusion.CQRS.Contexts.DHT, as: DHTContext
  alias Effusion.DHT.KRPC.{Query, Response}

  def start_link(opts) do
    port = Keyword.fetch!(opts, :port)
    GenServer.start_link(__MODULE__, port, name: __MODULE__)
  end

  def init(port) do
    {:ok, _socket} = :gen_udp.open(port, active: :once)
    node_id = Base.decode64!(Application.fetch_env!(:effusion, :dht_node_id))
    {:ok, %{node_id: node_id, port: port}}
  end

  def handle_info({:udp, socket, ip, port, packet}, %{node_id: node_id} = state) do
    {:ok, de_bencoded} = Bento.decode(packet)

    context = %{
      current_timestamp: DateTime.utc_now(),
      local_node_id: node_id,
      remote_address: {ip, in_port_no}
    }

    case Map.get(de_bencoded, "y") do
      "q" ->
        Task.start(fn ->
          Effusion.DHT.Handler.handle_krpc_query,
            Query.decode(de_bencoded),
            context
          )
          |> Response.encode()
          |> Bento.encode!()
          _ = :gen_udp.send(socket, ip, port, response)
        end)
      "r" ->
        Task.start(
          Effusion.DHT.Handler,
          :handle_krpc_response,
          [
            Response.decode(de_bencoded),
            context
          ]
        )
    end
    {:noreply, state}
  end

  def handle_info({:udp_passive, socket}, state) do
    :ok = :inet.setopts(socket, active: :once)
    {:noreply, state}
  end
end
