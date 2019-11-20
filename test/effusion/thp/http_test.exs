defmodule Effusion.THP.HTTPTest do
  use ExUnit.Case
  alias Effusion.THP.HTTP
  import Mox
  doctest Effusion.THP.HTTP

  @body File.read!("test/tracker_response.txt")

  test "decodes interval" do
    {:ok, body} = HTTP.decode(@body)

    assert Map.has_key?(body, :interval)

    assert is_integer(body.interval)
    assert body.interval > 0
  end

  test "decodes peers" do
    {:ok, body} = HTTP.decode(@body)
    assert Map.has_key?(body, :peers)

    [peer | _rest] = body.peers

    assert peer != nil
  end

  test "decodes peer port" do
    {:ok, body} = HTTP.decode(@body)

    [peer | _rest] = body.peers

    assert Map.has_key?(peer, :port)
    assert is_integer(peer.port)
  end

  test "decodes peer ip address" do
    {:ok, body} = HTTP.decode(@body)

    [peer | _rest] = body.peers

    assert Map.has_key?(peer, :ip)
    assert {a, b, c, d} = peer.ip
    assert is_integer(a)
    assert is_integer(b)
    assert is_integer(c)
    assert is_integer(d)
  end

  test "decodes peer id" do
    {:ok, body} = HTTP.decode(@body)

    [peer | _rest] = body.peers

    assert Map.has_key?(peer, :peer_id)
  end

  test "sends STARTED event" do
    Effusion.HTTP.Mock
    |> expect(:get, fn uri ->

      [_host, query] = String.split(uri, "?")
      query = URI.decode_query(query)

      assert query["info_hash"] == "12345678901234567890"
      assert query["peer_id"] == "12345678901234567890"
      assert query["port"] == "8001"
      assert query["uploaded"] == "0"
      assert query["downloaded"] == "0"
      assert query["left"] == "0"
      assert query["event"] == "started"
      assert query["ip"] == "192.168.1.1"
      assert query["trackerid"] == "this is my tracker id"

      {:ok,
        %{body: @body, status_code: 200, headers: %{"content-length" => (byte_size(@body) |> to_string())}}
      }
    end)

    {:ok, res} =
      HTTP.announce(
        "http://localhost:9000/announce",
        {192, 168, 1, 1},
        8001,
        "12345678901234567890",
        "12345678901234567890",
        0,
        0,
        0,
        event: :started,
        trackerid: "this is my tracker id",
        http_client: Effusion.HTTP.Mock
      )

    assert res.interval == 1800
  end
end
