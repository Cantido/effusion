defmodule Effusion.DHT.ProtocolHandler do
  alias Effusion.CQRS.Contexts.DHT, as: DHTContext
  alias Effusion.DHT
  alias Effusion.Repo
  import Bitwise
  import Effusion.DHT, only: [is_node_id: 1]
  import Ecto.Query
  require Logger

  @moduledoc """
  Handles incoming KRPC queries and responses.
  """

  def handle_krpc_query({:ping, transaction_id, _sender_id}, context) do
    {:ping, transaction_id, context.local_node_id}
  end

  def handle_krpc_query({:find_node, transaction_id, sender_id, target_id}, context)
    when is_node_id(sender_id)
     and is_node_id(target_id) do
    nodes = target_id
      |> closest_nodes()
      |> Enum.map(&Node.compact/1)
    {:find_node, transaction_id, context.local_node_id, nodes}
  end

  def handle_krpc_query({:get_peers, transaction_id, sender_id, info_hash},
                        %{remote_address: {host, port}, current_timestamp: now, local_node_id: local_node_id}) do

    token = DHT.token()

    :ok = DHTContext.issue_token(token, host, port)

    nodes = closest_nodes(info_hash)
    matching_nodes = Enum.filter(nodes, fn node ->
      node.node_id == info_hash
    end)

    if Enum.empty?(matching_nodes) do
      nodes = Enum.map(nodes, &Node.compact/1)
      {:get_peers_nearest, transaction_id, local_node_id, token, nodes}
    else
      matching_peers = Enum.map(matching_nodes, &compact_peer(&1.host, &1.port))
      {:get_peers_matching, transaction_id, local_node_id, token, matching_peers}
    end
  end

  def handle_krpc_query({:announce_peer, transaction_id, sender_id, _info_hash, _port, _token}, %{local_node_id: local_node_id, current_timestamp: now}) do
    token_query =
      from node in Node,
      where: node.node_id == ^sender_id,
      select: {node.sent_token, node.sent_token_timestamp}

    case Repo.one(token_query) do
      {_token, timestamp} ->
        token_expiry = timestamp |> Timex.shift(minutes: 15)
        if Timex.before?(now, token_expiry) do
          {:announce_peer, transaction_id, local_node_id}
        else
          {:error, [203, "token expired"]}
        end
      nil ->
        {:error, [203, "token not recognized"]}
    end
  end

  def handle_krpc_query({:announce_peer, transaction_id, _sender_id, _info_hash, _port, _token, :implied_port}, context) do
    {:announce_peer, transaction_id, context.local_node_id}
  end

  defp closest_nodes(target_id) when is_node_id(target_id) do
    <<target_id_int::160>> = target_id
    Repo.all(Node)
    |> Enum.sort_by(fn node ->
      <<node_id::160>> = Effusion.Hash.decode(node.node_id)
      DHT.distance(node_id, target_id_int)
    end)
    |> Enum.take(8)
  end

  def handle_krpc_response({:ping, _transaction_id, node_id}, %{current_timestamp: now}) do
    :ok
  end

  def handle_krpc_response(
    {:find_node, _transaction_id, _node_id, nodes},
    %{local_node_id: local_node_id}
  ) when is_node_id(local_node_id) do
    nodes
    |> Enum.map(fn {node_id, {host, port}} ->
      DHTContext.add_node(local_node_id, node_id, host, port)
    end)
    :ok
  end

  def handle_krpc_response(
    {:get_peers_matching, response_transaction_id, node_id, token, peers},
    _context
  ) do
    DHTContext.handle_peers_matching(node_id, response_transaction_id, token, peers)
  end

  def handle_krpc_response({:get_peers_nearest, _transaction_id, _node_id, token, nodes}, _context) do
    nodes_to_insert = Enum.map(nodes, fn {node_id, {ip, port}} ->
      %{
        node_id: node_id,
        address: ip,
        port: port,
        received_token: token,
        last_contacted: DateTime.utc_now()
      }
    end)
    Repo.insert_all(Node, nodes_to_insert)

    :ok
  end

  def handle_krpc_response({:announce_peer, _transaction_id, _node_id}, _context) do
    :ok
  end

  def compact_peer(host, port) do
    {ip0, ip1, ip2, ip3} = host
    <<ip0, ip1, ip2, ip3, port::16>>
  end
end
