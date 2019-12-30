defmodule Effusion.DHT.ServerTest do
  use ExUnit.Case
  alias Effusion.DHT.Server
  alias Effusion.Repo
  import Ecto.Query

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
  end

  @node_id Application.get_env(:effusion, :dht_node_id) |> Base.decode64!()
  @address {{10, 0, 0, 1}, 6969}

  test "ping query" do
    {:ping, transaction_id, node_id} = Server.handle_krpc_query({:ping, "abcde", "12345678901234567890"}, @address)
    assert transaction_id == "abcde"
    assert node_id == @node_id
  end

  test "handle find_node query" do
    {:ok, node} = Repo.insert(%Effusion.DHT.Node{
      node_id: "09876543210987654321",
      address: %Postgrex.INET{address: {192, 168, 1, 1}},
      port: 65535,
      last_contacted: DateTime.truncate(Timex.now(), :second)
    })
    node = Effusion.DHT.Node.compact(node)

    query = {:find_node, "abcde", "12345678901234567890", "09876543210987654321"}
    {:find_node, transaction_id, node_id, nodes} = Server.handle_krpc_query(query, @address)

    assert transaction_id == "abcde"
    assert node_id == @node_id
    assert nodes == [node]
  end

  test "handle get_peers query with matching peers" do
    {:ok, node} = Repo.insert(%Effusion.DHT.Node{
      node_id: "09876543210987654321",
      address: %Postgrex.INET{address: {192, 168, 1, 2}},
      port: 65535,
      last_contacted: DateTime.truncate(Timex.now(), :second)
    })
    peer = Effusion.BTP.Peer.compact(%{
      address: %Postgrex.INET{address: {192, 168, 1, 2}},
      port: 65535
    })

    query = {:get_peers, "abcde", "12345678901234567890", "09876543210987654321"}
    {:get_peers_matching, transaction_id, node_id, token, peers} = Server.handle_krpc_query(query, @address)

    assert peers == [peer]
  end

  test "get_peers saves token" do
    {:ok, node} = Repo.insert(%Effusion.DHT.Node{
      node_id: "09876543210987654321",
      address: %Postgrex.INET{address: {192, 168, 1, 2}},
      port: 65535,
      last_contacted: DateTime.truncate(Timex.now(), :second)
    })
    peer = Effusion.BTP.Peer.compact(%{
      address: %Postgrex.INET{address: {192, 168, 1, 2}},
      port: 65535
    })

    query = {:get_peers, "abcde", "12345678901234567890", "09876543210987654321"}
    response = Server.handle_krpc_query(query, @address)
    {:get_peers_matching, _transaction_id, _node_id, token, _matching_peers} = response

    {actual_token, actual_timestamp} = Repo.one!(from node in Effusion.DHT.Node,
                                        where: node.node_id == ^"12345678901234567890",
                                        select: {node.sent_token, node.sent_token_timestamp})
    assert token == actual_token
  end

  test "handle get_peers query with nearest nodes" do
    {:ok, node} = Repo.insert(%Effusion.DHT.Node{
      node_id: "abcdefghij1234567890",
      address: %Postgrex.INET{address: {192, 168, 1, 123}},
      port: 7070,
      last_contacted: DateTime.truncate(Timex.now(), :second)
    })
    node = Effusion.DHT.Node.compact(node)

    query = {:get_peers, "abcde", "12345678901234567890", "09876543210987654321"}
    {:get_peers_nearest, transaction_id, node_id, token, nodes} = Server.handle_krpc_query(query, @address)

    assert node in nodes
  end

  test "handle announce_peer query rejects queries for tokens it hasn't seen before" do
    {:error, [203, "token not recognized"]} = Server.handle_krpc_query({
      :announce_peer,
      "abcde",
      "12345678901234567890",
      "info hash!!~~~~~~~~~",
      6969,
      "token!"
      }, @address)
  end

  test "handle announce_peer query rejects queries for expired tokens" do
    Repo.insert(%Effusion.DHT.Node{
      node_id: "12345678901234567890",
      address: %Postgrex.INET{address: {192, 168, 1, 2}},
      port: 6969,
      sent_token: "abcde",
      sent_token_timestamp: Timex.shift(Timex.now(), minutes: -20) |> DateTime.truncate(:second),
      last_contacted: Timex.shift(Timex.now(), minutes: -20) |> DateTime.truncate(:second)
    })

    {:error, [203, "token expired"]} = Server.handle_krpc_query({
      :announce_peer,
      "abcde",
      "12345678901234567890",
      "info hash!!~~~~~~~~~",
      6969,
      "token!"
      }, @address)
  end

  test "handle announce_peer query success" do
    Repo.insert(%Effusion.DHT.Node{
      node_id: "12345678901234567890",
      address: %Postgrex.INET{address: {192, 168, 1, 2}},
      port: 6969,
      sent_token: "abcde",
      sent_token_timestamp: Timex.now() |> DateTime.truncate(:second),
      last_contacted: Timex.now() |> DateTime.truncate(:second)
    })

    {:announce_peer, "abcde", @node_id} = Server.handle_krpc_query({
      :announce_peer,
      "abcde",
      "12345678901234567890",
      "info hash!!~~~~~~~~~",
      6969,
      "token!"
      }, @address)
  end
end
