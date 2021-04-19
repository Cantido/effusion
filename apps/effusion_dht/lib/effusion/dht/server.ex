defmodule Effusion.DHT.Server do
  alias Effusion.DHT
  alias Effusion.DHT.KRPC
  alias Effusion.DHT.Node
  alias Effusion.DHT.Peer
  alias Effusion.DHT.Table

  require Logger

  @enforce_keys [
    :table
  ]
  defstruct [
    table: nil,
    tokens: %{}
  ]

  def new(local_id) do
    %__MODULE__{
      table: Table.new(local_id)
    }
  end

  def handle_message(state, message = %{"q" => "ping"}, context) do
    node_id = context.node_id
    ip = context.ip
    port = context.port

    response_params = %{
      id: node_id,
    }

    response = KRPC.new_response(message["t"], response_params)


    node = %Node{
      id: message["a"]["id"],
      host: ip,
      port: port
    }

    state =
      Map.update!(
        state,
        :table,
        &Table.add(&1, node)
      )
    {response, state}
  end

  def handle_message(state, message = %{"q" => "get_peers"}, context) do
    node_id = context.node_id
    ip = context.ip

    info_hash = message["a"]["info_hash"]

    peers = Effusion.PWP.get_peers_for_download(info_hash)

    if Enum.empty?(peers) do
      nodes =
        Map.get(state, :table)
        |> Table.take_closest_to(info_hash, 8)
        |> Enum.map(&Node.compact/1)
        |> Enum.join()

      token = DHT.generate_announce_peer_token()

      response_params = %{
        id: node_id,
        token: token,
        nodes: nodes
      }

      response = KRPC.new_response(message["t"], response_params)

      tokens =
        Map.get(state, :tokens, %{})
        |> Map.put(ip, token)

      state = Map.put(state, :tokens, tokens)
      {response, state}
    else
      peers =
        peers
        |> Enum.map(&Peer.compact/1)
        |> Enum.join()

      token = DHT.generate_announce_peer_token()

      response_params = %{
        id: node_id,
        token: token,
        values: peers
      }

      response = KRPC.new_response(message["t"], response_params)

      tokens =
        Map.get(state, :tokens, %{})
        |> Map.put(ip, token)

      state = Map.put(state, :tokens, tokens)

      {response, state}
    end
  end

  def handle_message(state, message = %{"q" => "find_node"}, context) do
    node_id = context.node_id

    target = message["a"]["target"]

    nodes =
      Map.get(state, :table)
      |> Table.take_closest_to(target, 8)
      |> Enum.map(&Node.compact/1)
      |> Enum.join()

    response_params = %{
      id: node_id,
      nodes: nodes
    }

    response = KRPC.new_response(message["t"], response_params)

    {response, state}
  end

  def handle_message(state, message = %{"q" => "announce_peer"}, context) do
    node_id = context.node_id
    ip = context.ip
    port = context.port

    expected_token =
      Map.get(state, :tokens, %{})
      |> Map.get(ip)

    if is_nil(expected_token) or message["a"]["token"] != expected_token do
      response = KRPC.new_error(message["t"], [203, "Bad token"])

      {response, state}
    else
      response_params = %{
        id: node_id,
      }

      response = KRPC.new_response(message["t"], response_params)

      peer_port =
        if message["a"]["implied_port"] == 1 do
          port
        else
          message["a"]["port"]
        end

      :ok =
        Effusion.PWP.add(
          message["a"]["info_hash"],
          ip,
          peer_port,
          "dht"
        )

      tokens = Map.delete(state.tokens, ip)
      {response, %{state | tokens: tokens}}
    end
  end

  def handle_message(state, message = %{"q" => query}, _context) do
    Logger.error("Unrecognized message: #{inspect message}")

    response = KRPC.new_error(message["t"], [204, "Unknown method #{query}"])

    {response, state}
  end
end