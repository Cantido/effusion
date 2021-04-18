defmodule Effusion.DHT.NodeTest do
  use ExUnit.Case, async: true
  alias Effusion.DHT.Node
  doctest Effusion.DHT.Node

  describe "query_received_at/2" do
    test "updates last_query_recieved_at" do
      node =
        Node.generate_node_id()
        |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))
        |> Node.query_received_at(~U[2021-04-18T14:11:00Z])

      assert DateTime.compare(node.last_query_received_at, ~U[2021-04-18T14:11:00Z]) == :eq
    end

    test "only updates timestamp if it is later than the current one" do
      node =
        Node.generate_node_id()
        |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))
        |> Node.query_received_at(~U[2021-04-18T14:11:00Z])
        |> Node.query_received_at(~U[2021-04-17T14:11:00Z])

      assert DateTime.compare(node.last_query_received_at, ~U[2021-04-18T14:11:00Z]) == :eq
    end
  end

  describe "response_received_at/2" do
    test "updates last_response_recieved_at" do
      node =
        Node.generate_node_id()
        |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))
        |> Node.response_received_at(~U[2021-04-18T14:11:00Z])
        |> Node.response_received_at(~U[2021-04-17T14:11:00Z])

      assert DateTime.compare(node.last_response_received_at, ~U[2021-04-18T14:11:00Z]) == :eq
    end
  end

  describe "status/2" do
    test "returns :unknown if we've never heard from the node ever" do
      status =
        Node.generate_node_id()
        |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))
        |> Node.status(~U[2021-04-18T12:14:59Z])

      assert status == :unknown
    end

    test "returns :good if it has responded within the last fifteen minutes" do
      status =
        Node.generate_node_id()
        |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))
        |> Node.response_received_at(~U[2021-04-18T12:00:00Z])
        |> Node.status(~U[2021-04-18T12:14:59Z])

      assert status == :good
    end

    test "returns :good if it has ever responded and queried us within the last fifteen minutes" do
      status =
        Node.generate_node_id()
        |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))
        |> Node.response_received_at(~U[2021-04-17T12:00:00Z])
        |> Node.query_received_at(~U[2021-04-18T12:00:00Z])
        |> Node.status(~U[2021-04-18T12:14:59Z])

      assert status == :good
    end

    test "returns :questionable if it has been more than fifteen minutes since the response or query" do
      status =
        Node.generate_node_id()
        |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))
        |> Node.response_received_at(~U[2021-04-18T12:00:00Z])
        |> Node.query_received_at(~U[2021-04-18T12:00:00Z])
        |> Node.status(~U[2021-04-18T12:15:01Z])

      assert status == :questionable
    end

    test "returns :bad if it has failed to respond to five or more queries" do
      status =
        Node.generate_node_id()
        |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))
        |> Node.response_received_at(~U[2021-04-18T12:00:00Z])
        |> Node.failed_query()
        |> Node.failed_query()
        |> Node.failed_query()
        |> Node.failed_query()
        |> Node.failed_query()
        |> Node.status(~U[2021-04-18T12:20:00Z])

      assert status == :bad
    end
  end

  test "time_since_last_response returns a positive value" do
    seconds =
      Node.generate_node_id()
      |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))
      |> Node.response_received_at(~U[2021-04-18T12:00:00Z])
      |> Node.time_since_last_response(~U[2021-04-18T12:00:01Z], :second)

    assert seconds == 1
  end

  test "time_since_last_query returns a positive value" do
    seconds =
      Node.generate_node_id()
      |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))
      |> Node.query_received_at(~U[2021-04-18T12:00:00Z])
      |> Node.time_since_last_query(~U[2021-04-18T12:00:01Z], :second)

    assert seconds == 1
  end
end
