defmodule Effusion.DHT.KRPC.QueryTest do
  use ExUnit.Case
  alias Effusion.DHT.KRPC.Query
  doctest Effusion.DHT.KRPC.Query

  def assert_transaction_id(id) do
    assert is_binary(id)
    assert List.ascii_printable?(String.to_charlist(id))
    assert byte_size(id) == 2
  end

  describe "ping/1" do
    test "contains transaction ID" do
      msg = Query.encode({:ping, "aa", "12345678901234567890"})

      assert_transaction_id(msg["t"])
    end

    test "sets type to 'q'" do
      msg = Query.encode({:ping, "aa", "12345678901234567890"})

      assert msg["y"] == "q"
    end

    test "sets query to 'ping'" do
      msg = Query.encode({:ping, "aa", "12345678901234567890"})

      assert msg["q"] == "ping"
    end

    test "sets ID argument to node ID" do
      msg = Query.encode({:ping, "aa", "12345678901234567890"})

      assert msg["a"] == %{"id" => "12345678901234567890"}
    end
  end

  describe "find_node/2" do
    test "contains transaction ID" do
      msg = Query.encode({:find_node, "aa", "12345678901234567890", "09876543210987654321"})

      assert_transaction_id(msg["t"])
    end

    test "sets type to 'q'" do
      msg = Query.encode({:find_node, "aa", "12345678901234567890", "09876543210987654321"})

      assert msg["y"] == "q"
    end

    test "sets query to 'find_node'" do
      msg = Query.encode({:find_node, "aa", "12345678901234567890", "09876543210987654321"})

      assert msg["q"] == "find_node"
    end

    test "sets 'id' argument to node ID" do
      msg = Query.encode({:find_node, "aa", "12345678901234567890", "09876543210987654321"})

      assert msg["a"]["id"] == "12345678901234567890"
    end

    test "sets 'target' argument to target ID" do
      msg = Query.encode({:find_node, "aa", "12345678901234567890", "09876543210987654321"})

      assert msg["a"]["target"] == "09876543210987654321"
    end
  end

  describe "get_peers/2" do
    test "contains transaction ID" do
      msg = Query.encode({:get_peers, "aa", "12345678901234567890", "09876543210987654321"})

      assert_transaction_id(msg["t"])
    end

    test "sets type to 'q'" do
      msg = Query.encode({:get_peers, "aa", "12345678901234567890", "09876543210987654321"})

      assert msg["y"] == "q"
    end

    test "sets query to 'get_peers'" do
      msg = Query.encode({:get_peers, "aa", "12345678901234567890", "09876543210987654321"})

      assert msg["q"] == "get_peers"
    end

    test "sets 'id' argument to node ID" do
      msg = Query.encode({:get_peers, "aa", "12345678901234567890", "09876543210987654321"})

      assert msg["a"]["id"] == "12345678901234567890"
    end

    test "sets 'info_hash' argument to info hash" do
      msg = Query.encode({:get_peers, "aa", "12345678901234567890", "09876543210987654321"})

      assert msg["a"]["info_hash"] == "09876543210987654321"
    end
  end

  describe "announce_peer/2" do
    test "contains transaction ID" do
      msg = Query.encode({:announce_peer, "aa", "12345678901234567890", "09876543210987654321", 65_535, "abcdefg"})

      assert_transaction_id(msg["t"])
    end

    test "sets type to 'q'" do
      msg = Query.encode({:announce_peer, "aa", "12345678901234567890", "09876543210987654321", 65_535, "abcdefg"})

      assert msg["y"] == "q"
    end

    test "sets query to 'get_peers'" do
      msg = Query.encode({:announce_peer, "aa", "12345678901234567890", "09876543210987654321", 65_535, "abcdefg"})

      assert msg["q"] == "announce_peer"
    end

    test "sets 'id' argument to node ID" do
      msg = Query.encode({:announce_peer, "aa", "12345678901234567890", "09876543210987654321", 65_535, "abcdefg"})

      assert msg["a"]["id"] == "12345678901234567890"
    end

    test "sets 'info_hash' argument to info hash" do
      msg = Query.encode({:announce_peer, "aa", "12345678901234567890", "09876543210987654321", 65_535, "abcdefg"})

      assert msg["a"]["info_hash"] == "09876543210987654321"
    end

    test "sets 'port' argument to port" do
      msg = Query.encode({:announce_peer, "aa", "12345678901234567890", "09876543210987654321", 65_535, "abcdefg"})

      assert msg["a"]["port"] == 65_535
    end

    test "sets 'token' argument to token" do
      msg = Query.encode({:announce_peer, "aa", "12345678901234567890", "09876543210987654321", 65_535, "abcdefg"})

      assert msg["a"]["token"] == "abcdefg"
    end

    test "sets 'implied_port' to zero by default" do
      msg = Query.encode({:announce_peer, "aa", "12345678901234567890", "09876543210987654321", 65_535, "abcdefg"})

      assert msg["a"]["implied_port"] == 0
    end

    test "sets 'implied_port' if arg is given" do
      msg = Query.encode({:announce_peer, "aa", "12345678901234567890", "09876543210987654321", 65_535, "abcdefg", :implied_port})

      assert msg["a"]["implied_port"] == 1
    end
  end

  test "decode ping" do
    decoded = Query.decode(%{
      "t" => "aa",
      "y" => "q",
      "q" => "ping",
      "a" => %{"id" => "12345678901234567890"}
    })
    assert decoded == {:ping, "aa", "12345678901234567890"}
  end

  test "decode find_node" do
    decoded = Query.decode(%{
      "t" => "aa",
      "y" => "q",
      "q" => "find_node",
      "a" => %{"id" => "12345678901234567890", "target" => "09876543210987654321"}
    })
    assert decoded == {:find_node, "aa", "12345678901234567890", "09876543210987654321"}
  end

  test "decode get_peers" do
    decoded = Query.decode(%{
      "t" => "aa",
      "y" => "q",
      "q" => "get_peers",
      "a" => %{"id" => "12345678901234567890", "info_hash" => "09876543210987654321"}
    })
    assert decoded == {:get_peers, "aa", "12345678901234567890", "09876543210987654321"}
  end

  test "decode announce_peer without implied_port" do
    decoded = Query.decode(%{
      "t" => "aa",
      "y" => "q",
      "q" => "announce_peer",
      "a" => %{
        "id" => "12345678901234567890",
        "info_hash" => "09876543210987654321",
        "port" => 65535,
        "token" => "abcdef",
        "implied_port" => 0
      }
    })
    assert decoded == {:announce_peer, "aa", "12345678901234567890", "09876543210987654321", 65535, "abcdef"}
  end

  test "decode announce_peer with implied_port" do
    decoded = Query.decode(%{
      "t" => "aa",
      "y" => "q",
      "q" => "announce_peer",
      "a" => %{
        "id" => "12345678901234567890",
        "info_hash" => "09876543210987654321",
        "port" => 65535,
        "token" => "abcdef",
        "implied_port" => 1
      }
    })
    assert decoded == {:announce_peer, "aa", "12345678901234567890", "09876543210987654321", 65535, "abcdef", :implied_port}
  end
end
