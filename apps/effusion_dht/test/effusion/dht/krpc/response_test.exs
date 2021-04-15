defmodule Effusion.DHT.KRPC.ResponseTest do
  use ExUnit.Case
  alias Effusion.DHT.KRPC.Response
  doctest Effusion.DHT.KRPC.Response

  test "ping" do
    response = Response.encode({:ping, "aa", "12345678901234567890"})

    assert response == %{
      "t" => "aa",
      "y" => "r",
      "q" => "ping",
      "r" => %{
        "id" => "12345678901234567890"
      }
    }
  end

  test "find_node" do
    node = {"09876543210987654321", {{192, 168, 1, 1}, 65535}}
    response = Response.encode({:find_node, "aa", "12345678901234567890", [node]})

    assert response == %{
      "t" => "aa",
      "y" => "r",
      "q" => "find_node",
      "r" => %{
        "id" => "12345678901234567890",
        "nodes" => "09876543210987654321" <> <<192, 168, 1, 1, 255, 255>>
      }
    }
  end

  test "get_peers with matching peer" do
    peer1 = {{192, 168, 1, 1}, 65535}
    peer2 = {{192, 168, 1, 2}, 65535}
    response = Response.encode({:get_peers_matching, "aa", "12345678901234567890", "abcdef", [peer1 , peer2]})

    assert response == %{
      "t" => "aa",
      "y" => "r",
      "q" => "get_peers",
      "r" => %{
        "id" => "12345678901234567890",
        "token" => "abcdef",
        "values" => [<<192, 168, 1, 1, 255, 255>>, <<192, 168, 1, 2, 255, 255>>]
      }
    }
  end

  test "get_peers with nearest peers" do
    peer1 = {"nearest neighbor 1~~", {{192, 168, 1, 1}, 65535}}
    peer2 = {"nearest neighbor 2~~", {{192, 168, 1, 2}, 65535}}
    response = Response.encode({:get_peers_nearest, "aa", "12345678901234567890", "abcdef", [peer1 , peer2]})

    assert response == %{
      "t" => "aa",
      "y" => "r",
      "q" => "get_peers",
      "r" => %{
        "id" => "12345678901234567890",
        "token" => "abcdef",
        "nodes" => "nearest neighbor 1~~" <> <<192, 168, 1, 1, 255, 255>> <> "nearest neighbor 2~~" <> <<192, 168, 1, 2, 255, 255>>
      }
    }
  end

  test "announce_peer" do
    response = Response.encode({:announce_peer, "aa", "12345678901234567890"})

    assert response == %{
      "t" => "aa",
      "y" => "r",
      "q" => "announce_peer",
      "r" => %{
        "id" => "12345678901234567890"
      }
    }
  end

  test "decode ping" do
    response = Response.decode(%{
      "t" => "aa",
      "y" => "r",
      "q" => "ping",
      "r" => %{
        "id" => "12345678901234567890"
      }
    })

    assert response == {:ping, "aa", "12345678901234567890"}
  end

  test "decode find_node" do
    response = Response.decode(%{
      "t" => "aa",
      "y" => "r",
      "q" => "find_node",
      "r" => %{
        "id" => "12345678901234567890",
        "nodes" => "09876543210987654321" <> <<192, 168, 1, 1, 255, 255>>
      }
    })

    assert response == {:find_node, "aa", "12345678901234567890", [{"09876543210987654321", {{192, 168, 1, 1}, 65535}}]}
  end

  test "decode get_peers_matching" do
    response = Response.decode(%{
      "t" => "aa",
      "y" => "r",
      "q" => "get_peers",
      "r" => %{
        "id" => "12345678901234567890",
        "token" => "abcdef",
        "values" => [<<192, 168, 1, 1, 255, 255>>, <<192, 168, 1, 2, 255, 255>>]
      }
    })

    assert response == {:get_peers_matching, "aa", "12345678901234567890", "abcdef", [{{192, 168, 1, 1}, 65535} , {{192, 168, 1, 2}, 65535}]}
  end

  test "decode get_peers_nearest" do
    response = Response.decode(%{
      "t" => "aa",
      "y" => "r",
      "q" => "get_peers",
      "r" => %{
        "id" => "12345678901234567890",
        "token" => "abcdef",
        "nodes" => "nearest neighbor 1~~" <> <<192, 168, 1, 1, 255, 255>> <> "nearest neighbor 2~~" <> <<192, 168, 1, 2, 255, 255>>
      }
    })

    assert response == {
      :get_peers_nearest,
      "aa",
      "12345678901234567890",
      "abcdef",
      [
        {"nearest neighbor 1~~", {{192, 168, 1, 1}, 65535}},
        {"nearest neighbor 2~~", {{192, 168, 1, 2}, 65535}}]
      }
  end

  test "decode announce_peer" do
    response = Response.decode(%{
      "t" => "aa",
      "y" => "r",
      "q" => "announce_peer",
      "r" => %{
        "id" => "12345678901234567890"
      }
    })

    assert response == {:announce_peer, "aa", "12345678901234567890"}
  end
end
