defmodule Effusion.DHT.ProtocolHandlerTest do
  use ExUnit.Case
  alias Effusion.DHT.{Bucket, Node, ProtocolHandler}
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
          current_timestamp: ~U[2020-06-01 12:00:00Z]
        }
      }
    }
  end

  test "ping query", %{context: context} do
    {:ping, transaction_id, node_id} = ProtocolHandler.handle_krpc_query({:ping, "abcde", "12345678901234567890"}, context)
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
    {:find_node, transaction_id, node_id, nodes} = ProtocolHandler.handle_krpc_query(query, context)

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
    {:get_peers_matching, _transaction_id, _node_id, _token, peers} = ProtocolHandler.handle_krpc_query(query, context)

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
    response = ProtocolHandler.handle_krpc_query(query, context)
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
    {:get_peers_nearest, _transaction_id, _node_id, _token, nodes} = ProtocolHandler.handle_krpc_query(query, context)

    assert node in nodes
  end

  test "handle announce_peer query rejects queries for tokens it hasn't seen before", %{context: context} do
    {:error, [203, "token not recognized"]} = ProtocolHandler.handle_krpc_query({
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
      sent_token_timestamp: Timex.shift(context.current_timestamp, minutes: -20) |> DateTime.truncate(:second),
      last_contacted: Timex.shift(context.current_timestamp, minutes: -20) |> DateTime.truncate(:second)
    })

    {:error, [203, "token expired"]} = ProtocolHandler.handle_krpc_query({
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
    {:announce_peer, "abcde", ^local_node_id} = ProtocolHandler.handle_krpc_query({
      :announce_peer,
      "abcde",
      "12345678901234567890",
      "info hash!!~~~~~~~~~",
      6969,
      "token!"
      }, context)
  end

  test "handle ping response", %{context: context} do
    :ok = ProtocolHandler.handle_krpc_response({:ping, "abcde", "12345678901234567890"}, context)

    last_contacted = Repo.one!(
      from node in Node,
      where: node.node_id == ^"12345678901234567890",
      select: node.last_contacted
    )

    earliest_timestamp_allowed = Timex.shift(context.current_timestamp, minutes: -1)
    assert Timex.after?(last_contacted, earliest_timestamp_allowed)
    assert Timex.before?(last_contacted, DateTime.utc_now())
  end

  test "handle find_node response", %{context: context} do
    nodes = [{"abcdefghij1234567890", {{10, 0, 0, 69}, 4200}}]
    :ok = ProtocolHandler.handle_krpc_response({:find_node, "abcde", "12345678901234567890", nodes}, context)

    Repo.one!(
      from node in Node,
      where: node.node_id == ^"abcdefghij1234567890"
    )
  end

  describe "find_node when the target bucket is full" do
    test "splits the bucket before inserting the node if our local node ID is in the bucket", %{context: context} do
      [
        {<<1::160>>, {{127, 0, 0, 1}, 5000}, ~U[2020-06-01 11:30:00Z]},
        {<<2::160>>, {{127, 0, 0, 2}, 5000}, ~U[2020-06-01 12:00:00Z]},
        {<<3::160>>, {{127, 0, 0, 3}, 5000}, ~U[2020-06-01 12:00:00Z]},
        {<<4::160>>, {{127, 0, 0, 4}, 5000}, ~U[2020-06-01 12:00:00Z]},
        {<<(Node.max_node_id_value - 4)::160>>, {{127, 0, 0, 5}, 5000}, ~U[2020-06-01 12:00:00Z]},
        {<<(Node.max_node_id_value - 3)::160>>, {{127, 0, 0, 6}, 5000}, ~U[2020-06-01 12:00:00Z]},
        {<<(Node.max_node_id_value - 2)::160>>, {{127, 0, 0, 7}, 5000}, ~U[2020-06-01 12:00:00Z]},
        {<<(Node.max_node_id_value - 1)::160>>, {{127, 0, 0, 8}, 5000}, ~U[2020-06-01 12:00:00Z]}
      ]
      |> Enum.map(fn {node_id, {host, port}, last_contacted} ->
        Node.changeset(%Node{}, %{
          node_id: node_id,
          address: %Postgrex.INET{address: host},
          port: port,
          last_contacted: last_contacted
        })
      end)
      |> Enum.each(fn changeset ->
        Repo.insert!(changeset, on_conflict: :nothing)
      end)

      # our own node ID is in the context map, it's set to the highest possible node ID
      nodes = [{<<(Node.max_node_id_value - 5)::160>>, {{10, 0, 0, 69}, 4200}}]
      :ok = ProtocolHandler.handle_krpc_response({:find_node, "abcde", "12345678901234567890", nodes}, context)

      Repo.one!(
        from node in Node,
        where: node.node_id == ^<<(Node.max_node_id_value - 5)::160>>
      )

      buckets = Repo.all(Bucket)
      assert Enum.count(buckets) == 2
    end

    test "drops old nodes if our local node ID isn't in the bucket", %{context: context} do
      [
        {<<1::160>>, {{127, 0, 0, 1}, 5000}, ~U[2020-06-01 11:30:00Z]},
        {<<2::160>>, {{127, 0, 0, 2}, 5000}, ~U[2020-06-01 12:00:00Z]},
        {<<3::160>>, {{127, 0, 0, 3}, 5000}, ~U[2020-06-01 12:00:00Z]},
        {<<4::160>>, {{127, 0, 0, 4}, 5000}, ~U[2020-06-01 12:00:00Z]},
        {<<5::160>>, {{127, 0, 0, 5}, 5000}, ~U[2020-06-01 12:00:00Z]},
        {<<6::160>>, {{127, 0, 0, 6}, 5000}, ~U[2020-06-01 12:00:00Z]},
        {<<7::160>>, {{127, 0, 0, 7}, 5000}, ~U[2020-06-01 12:00:00Z]},
        {<<8::160>>, {{127, 0, 0, 8}, 5000}, ~U[2020-06-01 12:00:00Z]}
      ]
      |> Enum.map(fn {node_id, {host, port}, last_contacted} ->
        Node.changeset(%Node{}, %{
          node_id: node_id,
          address: %Postgrex.INET{address: host},
          port: port,
          last_contacted: last_contacted
        })
      end)
      |> Enum.each(fn changeset ->
        Repo.insert!(changeset, on_conflict: :nothing)
      end)

      Bucket.split(context.local_node_id)

      # our own node ID is in the context map, it's set to the highest possible node ID
      # node ID <<1::160>> should be dropped since it's the oldest
      nodes = [{<<9::160>>, {{10, 0, 0, 69}, 4200}}]
      :ok = ProtocolHandler.handle_krpc_response({:find_node, "abcde", "12345678901234567890", nodes}, context)

      Repo.one!(
        from node in Node,
        where: node.node_id == ^<<9::160>>
      )

      nodes = Repo.all(Node)
      # the old node was replaced, so we still have eight
      assert Enum.count(nodes) == 8
    end
  end
end
