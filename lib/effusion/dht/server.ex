defmodule Effusion.DHT.Server do
  alias Effusion.BTP.{Peer, Torrent}
  alias Effusion.DHT
  alias Effusion.DHT.{Bucket, Node}
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

    case Repo.one(from node in Node, where: node.node_id == ^sender_id) do
      nil ->
        Node.changeset(%Node{}, %{
          node_id: sender_id,
          address: host,
          port: port,
          sent_token: token,
          sent_token_timestamp: now,
          last_contacted: now
        }) |> Repo.insert!()
      node ->
        Node.changeset(node, %{
          sent_token: token,
          sent_token_timestamp: now,
          last_contacted: now
        }) |> Repo.update!()
    end

    nodes = closest_nodes(info_hash)
    matching_nodes = Enum.filter(nodes, fn node ->
      node.node_id == info_hash
    end)

    if Enum.empty?(matching_nodes) do
      nodes = Enum.map(nodes, &Node.compact/1)
      {:get_peers_nearest, transaction_id, local_node_id, token, nodes}
    else
      matching_peers = Enum.map(matching_nodes, &Peer.compact/1)
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
    # I really wish I could do this in the DB, it's way easier,
    # but Postgres does not have a bitwise XOR operator for numerics
    <<target_id_int::160>> = target_id
    Repo.all(Node)
    |> Enum.map(fn node ->
      <<node_id::160>> = node.node_id
      {bxor(node_id, target_id_int), node}
    end)
    |> Enum.sort_by(fn {distance, _node} ->
      distance
    end)
    |> Enum.take(8)
    |> Enum.map(&elem(&1, 1))
  end

  def handle_krpc_response({:ping, _transaction_id, node_id}, %{remote_address: {host, port}, current_timestamp: now}) do
    case Repo.one(from node in Node, where: node.node_id == ^node_id) do
      nil ->
        Node.changeset(%Node{}, %{
          node_id: node_id,
          address: host,
          port: port,
          last_contacted: now
        }) |> Repo.insert()
      node ->
        Node.changeset(node, %{
          last_contacted: now
        }) |> Repo.update()
    end
    :ok
  end

  def handle_krpc_response({:find_node, _transaction_id, _node_id, nodes}, %{local_node_id: local_node_id, current_timestamp: now}) when is_node_id(local_node_id) do
    <<local_node_id_int::160>> = local_node_id
    Logger.debug("local node ID: #{inspect local_node_id_int}")
    nodes
    |> Enum.map(fn {node_id, {host, port}} ->
      bucket = Bucket.for_node_id(node_id) |> Repo.one!() |> Repo.preload(:nodes)
      bucket_size = Enum.count(bucket.nodes)

      if bucket_size >= 8 do
        # when bucket is full
        # does our own node ID fit within this bucket?
        our_bucket = Bucket.for_node_id(local_node_id) |> Repo.one!()

        if bucket.id == our_bucket.id do
          Logger.debug("Node exists in our own bucket (we are #{inspect local_node_id_int}); splitting bucket #{inspect bucket.range}")
          Bucket.split(local_node_id)
          insert_node({node_id, {host, port}})
        else
          # ping all older nodes in the bucket, see if they are still active, or drop the node we're inserting if they're all still active
          deleted_buckets_count = bucket.nodes
          |> Enum.filter(&Node.expired?(&1, now))
          |> Enum.filter(fn node ->
            Logger.debug("Expired node: #{inspect node}")
            # TODO: ping the node, drop them if they don't respond
            # returning true right now means we drop this old node
            true
          end)
          |> Enum.map(&Repo.delete!/1)
          |> Enum.count()

          if deleted_buckets_count > 0 do
            insert_node({node_id, {host, port}})
          end
          # If all our nodes are active, just drop what we're inserting
        end
      else
        # when bucket is not full
        insert_node({node_id, {host, port}})
      end
    end)
    :ok
  end

  def handle_krpc_response(
    {:get_peers_matching, response_transaction_id, _node_id, _token, peers},
    %{query: {:get_peers, query_transaction_id, _sender_id, info_hash}})
    when response_transaction_id == query_transaction_id do

    torrent_id =
      case Torrent.by_info_hash(info_hash) do
        {:ok, torrent}  -> torrent.id
        {:error, _} ->
          %Torrent{}
          |> Torrent.changeset(%{
            info_hash: info_hash,
            state: "dht_only"
          })
          |> Repo.insert()
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
        last_contacted: DateTime.utc_now()
      }
    end)
    Repo.insert_all(Node, nodes_to_insert)

    :ok
  end

  def handle_krpc_response({:announce_peer, _transaction_id, _node_id}, _context) do
    :ok
  end

  defp insert_node({node_id, {host, port}}) do
    Node.changeset(%Node{}, %{
      node_id: node_id,
      address: %Postgrex.INET{address: host},
      port: port
    })
    |> Repo.insert!(on_conflict: :nothing)
  end
end
