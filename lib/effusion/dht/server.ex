defmodule Effusion.DHT.Server do
  alias Effusion.BTP.{Torrent, Peer}
  alias Effusion.DHT
  alias Effusion.Repo
  import Bitwise
  import Effusion.DHT, only: [is_node_id: 1, is_info_hash: 1, is_inet_port: 1]
  import Ecto.Query

  @node_id Application.get_env(:effusion, :dht_node_id) |> Base.decode64!()

  def handle_krpc_query({:ping, transaction_id, sender_id}, _context) do
    {:ping, transaction_id, @node_id}
  end

  def handle_krpc_query({:find_node, transaction_id, sender_id, target_id}, _context)
    when is_node_id(sender_id)
     and is_node_id(target_id) do
    nodes = target_id
      |> closest_nodes()
      |> Enum.map(&DHT.Node.compact/1)
    {:find_node, transaction_id, @node_id, nodes}
  end

  defp closest_nodes(target_id) when is_node_id(target_id) do
    # I really wish I could do this in the DB, it's way easier,
    # but Postgres does not have a bitwise XOR operator.
    <<target_id_int::160>> = target_id
    Repo.all(DHT.Node)
    |> Enum.map(fn node ->
      <<node_id::160>> = node.node_id
      {bxor(node_id, target_id_int), node}
    end)
    |> Enum.sort_by(fn {distance, node} ->
      distance
    end)
    |> Enum.take(8)
    |> Enum.map(&elem(&1, 1))
  end

  def handle_krpc_query({:get_peers, transaction_id, sender_id, info_hash},
                        %{remote_address: {host, port}, current_timestamp: now}) do
    token = DHT.token()

    case Repo.one(from node in Effusion.DHT.Node, where: node.node_id == ^sender_id) do
      nil ->
        DHT.Node.changeset(%DHT.Node{}, %{
          node_id: sender_id,
          address: host,
          port: port,
          sent_token: token,
          sent_token_timestamp: now,
          last_contacted: now
        }) |> Repo.insert()
      node ->
        DHT.Node.changeset(node, %{
          sent_token: token,
          sent_token_timestamp: now,
          last_contacted: now
        }) |> Repo.update()
    end

    nodes = closest_nodes(info_hash)
    matching_nodes = Enum.filter(nodes, fn node ->
      node.node_id == info_hash
    end)

    if Enum.empty?(matching_nodes) do
      nodes = Enum.map(nodes, &DHT.Node.compact/1)
      {:get_peers_nearest, transaction_id, @node_id, token, nodes}
    else
      matching_peers = Enum.map(matching_nodes, &Peer.compact/1)
      {:get_peers_matching, transaction_id, @node_id, token, matching_peers}
    end
  end

  def handle_krpc_query({:announce_peer, transaction_id, sender_id, info_hash, port, token}, %{current_timestamp: now}) do
     token_query = from node in DHT.Node,
                   where: node.node_id == ^sender_id,
                   select: {node.sent_token, node.sent_token_timestamp}

    case Repo.one(token_query) do
      {token, timestamp} ->
        token_expiry = timestamp |> Timex.shift(minutes: 15)
        if Timex.before?(now, token_expiry) do
          {:announce_peer, transaction_id, @node_id}
        else
          {:error, [203, "token expired"]}
        end
      nil ->
        {:error, [203, "token not recognized"]}
    end
  end

  def handle_krpc_query({:announce_peer, transaction_id, sender_id, info_hash, port, token, :implied_port}, _context) do
    {:announce_peer, transaction_id, @node_id}
  end

  def handle_krpc_response({:ping, transaction_id, node_id}, %{remote_address: {host, port}, current_timestamp: now}) do
    case Repo.one(from node in Effusion.DHT.Node, where: node.node_id == ^node_id) do
      nil ->
        DHT.Node.changeset(%DHT.Node{}, %{
          node_id: node_id,
          address: host,
          port: port,
          last_contacted: now
        }) |> Repo.insert()
      node ->
        DHT.Node.changeset(node, %{
          last_contacted: now
        }) |> Repo.update()
    end
    :ok
  end

  def handle_krpc_response({:find_node, transaction_id, node_id, nodes}, _context) do
    nodes_to_insert = Enum.map(nodes, fn {node_id, {host, port}} ->
      %{
        node_id: node_id,
        address: %Postgrex.INET{address: host},
        port: port
      }
    end)
    Repo.insert_all(DHT.Node, nodes_to_insert, on_conflict: :nothing)
    :ok
  end

  def handle_krpc_response({:get_peers_matching, response_transaction_id, node_id, token, peers},
                           %{query: {:get_peers, query_transaction_id, _sender_id, info_hash}})
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
    :ok
  end

  def handle_krpc_response({:get_peers_nearest, _transaction_id, _node_id, token, nodes}, _context) do
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

    :ok
  end

  def handle_krpc_response({:announce_peer, transaction_id, node_id}, _context) do
    :ok
  end
end
