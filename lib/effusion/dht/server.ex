defmodule Effusion.DHT.Server do
  alias Effusion.DHT.{Node, ProtocolHandler}
  alias Effusion.DHT.KRPC.{Query, Response}
  alias Effusion.Repo
  use GenServer
  import Ecto.Query
  require Logger

  @node_id Base.decode64!(Application.get_env(:effusion, :dht_node_id))
  @port Application.get_env(:effusion, :port)

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  def init(opts) do
    port = Keyword.get(opts, :port, 8001)
    {:ok, socket} = :gen_udp.open(port, [:binary, {:active, :once}])
    {:ok, %{
      waiting_for_response: %{},
      port: port,
      socket: socket,
    }}
  end

  def handle_info({:udp, socket, ip, in_port_no, packet}, state) do
    Logger.debug("received DHT packet from #{inspect ip}")
    {:ok, de_bencoded} = Bento.decode(packet)

    context = %{
      current_timestamp: DateTime.utc_now(),
      local_node_id: @node_id,
      remote_address: {ip, in_port_no}
    }

    sender_id = de_bencoded["a"]["id"]

    Repo.one(
      from node in Node,
      where: node.node_id == ^sender_id
    )
    |> case do
      nil ->
        Node.changeset(%Node{}, %{
          address: ip,
          port: in_port_no,
          node_id: sender_id,
          last_contacted: DateTime.utc_now()
        })
        |> Repo.insert!()
      node -> node
        |> Ecto.Changeset.change(last_contacted: DateTime.utc_now())
        |> Repo.update!()
    end

    case Map.get(de_bencoded, "y") do
      "q" ->
        query = Query.decode(de_bencoded)
        response =
          ProtocolHandler.handle_krpc_query(query, context)
          |> Response.encode()

        encoded_response =
          response
          |> Bento.encode!()

        _ = :gen_udp.send(state.socket, ip, in_port_no, encoded_response)

        {:noreply, state}
      "r" ->
        transaction_id = de_bencoded["t"]
        if (Map.has_key?(state.waiting_for_response, transaction_id)) do
          responding_to = state.waiting_for_response[transaction_id]
          context = Map.put(context, :query, responding_to)

          :ok =
            de_bencoded
            |> Response.decode()
            |> ProtocolHandler.handle_krpc_response(context)

          state =
            Map.update!(
              state,
              :waiting_for_response,
              &Map.drop(&1, transaction_id)
            )

          {:noreply, state}
        else
          {:noreply, state}
        end
    end
  end

  def handle_info({:udp_passive, socket}, state) do
    :ok = :inet.setopts(socket, active: :once)
    {:noreply, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end
end
