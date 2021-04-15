defmodule Effusion.DHT.Server do
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
    now = DateTime.utc_now()
    context = %{
      current_timestamp: now,
      local_node_id: node_id,
      remote_address: {ip, port}
    }

    DHTContext.mark_node_as_contacted(node_id, now)

    {:ok, de_bencoded} = Bento.decode(packet)

    case Map.get(de_bencoded, "y") do
      "q" ->
        Task.start(fn ->
          response =
            Effusion.DHT.ProtocolHandler.handle_krpc_query(
              Query.decode(de_bencoded),
              context
            )
            |> Response.encode()
            |> Bento.encode!()
          _ = :gen_udp.send(socket, ip, port, response)
        end)
      "r" ->
          Effusion.DHT.ProtocolHandler.handle_krpc_response(
            Response.decode(de_bencoded),
            context
          )
    end
    {:noreply, state}
  end

  def handle_info({:udp_passive, socket}, state) do
    :ok = :inet.setopts(socket, active: :once)
    {:noreply, state}
  end
end
