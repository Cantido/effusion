defmodule Effusion.DHT.NodeTest do
  use ExUnit.Case, async: true
  alias Effusion.DHT.Node
  doctest Effusion.DHT.Node

  describe "new/3" do
    test "initializes consecutive_failed_queries to zero" do
      node =
        Node.generate_node_id()
        |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))

      assert node.consecutive_failed_queries == 0
    end

    test "initializes host to the given host argument" do
      node =
        Node.generate_node_id()
        |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))

      assert node.host == {127, 0, 0, 1}
    end

    test "initializes port to the given port argument" do
      port = Enum.random(1024..65535)

      node =
        Node.generate_node_id()
        |> Node.new({127, 0, 0, 1}, port)

      assert node.port == port
    end

    test "initializes id to the given id argument" do
      id = Node.generate_node_id()

      node = Node.new(id, {127, 0, 0, 1}, Enum.random(1024..65535))

      assert node.id == id
    end

    test "initializes last_query_received_at to nil" do
      node =
        Node.generate_node_id()
        |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))

      assert is_nil(node.last_query_received_at)
    end

    test "initializes last_response_received_at to nil" do
      node =
        Node.generate_node_id()
        |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))

      assert is_nil(node.last_response_received_at)
    end
  end

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

    test "resets consectuve_failed_queries" do
      node =
        Node.generate_node_id()
        |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))
        |> Node.failed_query()
        |> Node.response_received_at(~U[2021-04-18T14:11:00Z])

      assert node.consecutive_failed_queries == 0
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

  test "failed_query increments consecutive_failed_queries" do
    node =
      Node.generate_node_id()
      |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))
      |> Node.failed_query()

    assert node.consecutive_failed_queries == 1
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

  describe "ever_responded?/1" do
    test "returns false on a new node" do
      responded =
        Node.generate_node_id()
        |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))
        |> Node.ever_responded?()

      refute responded
    end

    test "returns true after we've received a response" do
      responded =
        Node.generate_node_id()
        |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))
        |> Node.response_received_at(~U[2021-04-18T12:00:00Z])
        |> Node.ever_responded?()

      assert responded
    end
  end

  describe "ever_queried?/1" do
    test "returns false on a new node" do
      queried =
        Node.generate_node_id()
        |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))
        |> Node.ever_queried?()

      refute queried
    end

    test "returns true after we've received a response" do
      queried =
        Node.generate_node_id()
        |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))
        |> Node.query_received_at(~U[2021-04-18T12:00:00Z])
        |> Node.ever_queried?()

      assert queried
    end
  end
end
