defmodule Effusion.DHT.ServerTest do
  use ExUnit.Case
  alias Effusion.DHT.{Bucket, Node, Server}
  alias Effusion.Repo
  import Ecto.Query

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Effusion.Repo, {:shared, self()})
  end

  setup do
    {:ok, bucket} = %Bucket{}
    |> Bucket.changeset(%{
      range: Bucket.initial_bucket_range()
    })
    |> Repo.insert()
    {:ok, %{bucket: bucket}}
  end

  setup do
    {
      :ok,
      %{
        context: %{
          local_node_id: Node.max_node_id_binary(),
          remote_address: {{10, 0, 0, 1}, 6969},
          current_timestamp: DateTime.utc_now()
        }
      }
    }
  end

  test "ping query", %{context: context} do
    {:ping, transaction_id, node_id} = Server.handle_krpc_query({:ping, "abcde", "12345678901234567890"}, context)
    assert transaction_id == "abcde"
    assert node_id == context.local_node_id
  end

  test "handle find_node query", %{context: context} do
    node = 
      Node.changeset(%Node{}, %{
        node_id: "09876543210987654321",
        address: %Postgrex.INET{address: {192, 168, 1, 1}},
        port: 65535,
        last_contacted: DateTime.truncate(DateTime.utc_now(), :second)
      })
      |> Repo.insert!()
    node = Node.compact(node)

    query = {:find_node, "abcde", "12345678901234567890", "09876543210987654321"}
    {:find_node, transaction_id, node_id, nodes} = Server.handle_krpc_query(query, context)

    assert transaction_id == "abcde"
    assert node_id == context.local_node_id
    assert nodes == [node]
  end

  test "handle get_peers query with matching peers", %{context: context, bucket: bucket} do
    {:ok, _node} = Repo.insert(%Node{
      node_id: "09876543210987654321",
      bucket_id: bucket.id,
      address: %Postgrex.INET{address: {192, 168, 1, 2}},
      port: 65535,
      last_contacted: DateTime.truncate(DateTime.utc_now(), :second)
    })
    peer = Effusion.BTP.Peer.compact(%{
      address: %Postgrex.INET{address: {192, 168, 1, 2}},
      port: 65535
    })

    query = {:get_peers, "abcde", "12345678901234567890", "09876543210987654321"}
    {:get_peers_matching, _transaction_id, _node_id, _token, peers} = Server.handle_krpc_query(query, context)

    assert peers == [peer]
  end

  test "get_peers saves token", %{context: context, bucket: bucket} do
    {:ok, _node} = Repo.insert(%Node{
      node_id: "09876543210987654321",
      bucket_id: bucket.id,
      address: %Postgrex.INET{address: {192, 168, 1, 2}},
      port: 65535,
      last_contacted: DateTime.truncate(DateTime.utc_now(), :second)
    })
    _peer = Effusion.BTP.Peer.compact(%{
      address: %Postgrex.INET{address: {192, 168, 1, 2}},
      port: 65535
    })

    query = {:get_peers, "abcde", "12345678901234567890", "09876543210987654321"}
    response = Server.handle_krpc_query(query, context)
    {:get_peers_matching, _transaction_id, _node_id, token, _matching_peers} = response

    {actual_token, _actual_timestamp} = Repo.one!(
      from node in Node,
      where: node.node_id == ^"12345678901234567890",
      select: {node.sent_token, node.sent_token_timestamp}
    )
    assert token == actual_token
  end

  test "handle get_peers query with nearest nodes", %{context: context, bucket: bucket} do
    {:ok, node} = Repo.insert(%Node{
      node_id: "abcdefghij1234567890",
      bucket_id: bucket.id,
      address: %Postgrex.INET{address: {192, 168, 1, 123}},
      port: 7070,
      last_contacted: DateTime.truncate(DateTime.utc_now(), :second)
    })
    node = Node.compact(node)

    query = {:get_peers, "abcde", "12345678901234567890", "09876543210987654321"}
    {:get_peers_nearest, _transaction_id, _node_id, _token, nodes} = Server.handle_krpc_query(query, context)

    assert node in nodes
  end

  test "handle announce_peer query rejects queries for tokens it hasn't seen before", %{context: context} do
    {:error, [203, "token not recognized"]} = Server.handle_krpc_query({
      :announce_peer,
      "abcde",
      "12345678901234567890",
      "info hash!!~~~~~~~~~",
      6969,
      "token!"
      }, context)
  end

  test "handle announce_peer query rejects queries for expired tokens", %{context: context, bucket: bucket} do
    Repo.insert(%Node{
      node_id: "12345678901234567890",
      bucket_id: bucket.id,
      address: %Postgrex.INET{address: {192, 168, 1, 2}},
      port: 6969,
      sent_token: "abcde",
      sent_token_timestamp: Timex.shift(DateTime.utc_now(), minutes: -20) |> DateTime.truncate(:second),
      last_contacted: Timex.shift(DateTime.utc_now(), minutes: -20) |> DateTime.truncate(:second)
    })

    {:error, [203, "token expired"]} = Server.handle_krpc_query({
      :announce_peer,
      "abcde",
      "12345678901234567890",
      "info hash!!~~~~~~~~~",
      6969,
      "token!"
      }, context)
  end

  test "handle announce_peer query success", %{context: context, bucket: bucket} do
    Repo.insert(%Node{
      node_id: "12345678901234567890",
      bucket_id: bucket.id,
      address: %Postgrex.INET{address: {192, 168, 1, 2}},
      port: 6969,
      sent_token: "abcde",
      sent_token_timestamp: DateTime.utc_now() |> DateTime.truncate(:second),
      last_contacted: DateTime.utc_now() |> DateTime.truncate(:second)
    })

    local_node_id = context.local_node_id
    {:announce_peer, "abcde", ^local_node_id} = Server.handle_krpc_query({
      :announce_peer,
      "abcde",
      "12345678901234567890",
      "info hash!!~~~~~~~~~",
      6969,
      "token!"
      }, context)
  end

  test "handle ping response", %{context: context} do
    :ok = Server.handle_krpc_response({:ping, "abcde", "12345678901234567890"}, context)

    last_contacted = Repo.one!(
      from node in Node,
      where: node.node_id == ^"12345678901234567890",
      select: node.last_contacted
    )

    earliest_timestamp_allowed = Timex.shift(DateTime.utc_now(), minutes: -1)
    assert Timex.after?(last_contacted, earliest_timestamp_allowed)
    assert Timex.before?(last_contacted, DateTime.utc_now())
  end

  test "handle find_node response", %{context: context} do
    nodes = [{"abcdefghij1234567890", {{10, 0, 0, 69}, 4200}}]
    :ok = Server.handle_krpc_response({:find_node, "abcde", "12345678901234567890", nodes}, context)

    Repo.one!(
      from node in Node,
      where: node.node_id == ^"abcdefghij1234567890"
    )
  end
end
