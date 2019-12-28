defmodule Effusion.DHT.Server do
  alias Effusion.BTP.{Torrent, Peer}
  alias Effusion.DHT
  alias Effusion.Repo
  import Bitwise
  import Effusion.DHT, only: [is_node_id: 1, is_info_hash: 1, is_inet_port: 1]
  import Ecto.Query

  @node_id Application.get_env(:effusion, :dht_node_id) |> Base.decode64!()

  def handle_krpc_query({:ping, transaction_id, sender_id}) do
    {:ping, transaction_id, @node_id}
  end

  def handle_krpc_query({:find_node, transaction_id, sender_id, target_id})
    when is_node_id(sender_id)
     and is_node_id(target_id) do
    <<target_id_int::160>> = target_id
    nodes = Repo.all(DHT.Node)
            |> Enum.map(fn node ->
              <<node_id::160>> = node.node_id
              {bxor(node_id, target_id_int), node}
            end)
            |> Enum.sort_by(fn {distance, node} ->
              distance
            end)
            |> Enum.take(8)
            |> Enum.map(&elem(&1, 1))
    nodes = Enum.map(nodes, &DHT.Node.compact/1)
    {:find_node, transaction_id, @node_id, nodes}
  end

  def handle_krpc_query({:get_peers, transaction_id, sender_id, info_hash}) do
    # Search our routing table for peers with a node_id close to info_hash
    # In postgres, '#' is the XOR operator
    nodes = Repo.all(from node in DHT.Node,
                     order_by: [asc: fragment("? # ?", node.node_id, ^info_hash)],
                     limit: 8)

    matching_nodes = Enum.filter(nodes, fn node ->
      node.node_id == info_hash
    end)


    if Enum.empty?(matching_nodes) do
      nodes = Enum.map(nodes, &DHT.Node.compact/1)
      {:get_peers_nearest, transaction_id, @node_id, DHT.token(), nodes}
    else
      matching_peers = Enum.map(matching_nodes, &Peer.compact/1)
      {:get_peers_matching, transaction_id, @node_id, DHT.token(), matching_peers}
    end
  end

  def handle_krpc_query({:announce_peer, transaction_id, sender_id, info_hash, port, token}) do
    {:announce_peer, transaction_id, @node_id}
  end

  def handle_krpc_query({:announce_peer, transaction_id, sender_id, info_hash, port, token, :implied_port}) do
    {:announce_peer, transaction_id, @node_id}
  end

  def handle_krpc_response({:ping, transaction_id, node_id}, _query) do

  end

  def handle_krpc_response({:find_node, transaction_id, node_id, nodes}, _query) do

  end

  def handle_krpc_response({:get_peers_matching, response_transaction_id, node_id, token, peers},
                           {:get_peers, query_transaction_id, _sender_id, info_hash})
    when response_transaction_id == query_transaction_id do

    torrent_id =
      case Torrent.by_info_hash(info_hash) do
        {:ok, torrent}  -> torrent.id
        {:error, _} ->
          %{info_hash: info_hash}
          |> Torrent.changeset()
          |> Repo.insert_or_update()
      end

    peers_to_insert = Enum.map(peers, fn {address, port} ->
      %{
        torrent_id: torrent_id,
        address: address,
        port: port
      }
    end)

    Repo.insert_all(Peer, peers_to_insert)
  end

  def handle_krpc_response({:get_peers_nearest, _transaction_id, _node_id, token, nodes}, _query) do
    nodes_to_insert = Enum.map(nodes, fn {node_id, {ip, port}} ->
      %{
        node_id: node_id,
        address: ip,
        port: port,
        received_token: token,
        last_contacted: Timex.now()
      }
    end)
    Repo.insert_all(DHT.Node, nodes_to_insert)

    # Issue another get_peers request to K closest nodes
  end

  def handle_krpc_response({:announce_peer, transaction_id, node_id}, _query) do

  end
end
