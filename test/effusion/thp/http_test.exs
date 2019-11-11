defmodule Effusion.THP.HTTPTest do
  use ExUnit.Case
  alias Effusion.THP.HTTP
  doctest Effusion.THP.HTTP

  @body File.read!("test/tracker_response.txt")

  setup do
    bypass = Bypass.open()
    {:ok, bypass: bypass}
  end

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

  test "sends STARTED event", %{bypass: bypass} do
    Bypass.expect_once(bypass, fn conn ->
      assert "/announce" == conn.request_path
      assert "GET" == conn.method
      query = Plug.Conn.fetch_query_params(conn).query_params

      assert query["info_hash"] == "12345678901234567890"
      assert query["peer_id"] == "12345678901234567890"
      assert query["port"] == "8001"
      assert query["uploaded"] == "0"
      assert query["downloaded"] == "0"
      assert query["left"] == "0"
      assert query["event"] == "started"
      assert query["ip"] == "192.168.1.1"
      assert query["trackerid"] == "this is my tracker id"

      Plug.Conn.resp(conn, 200, @body)
    end)

    {:ok, _} =
      HTTP.announce(
        "http://localhost:#{bypass.port}/announce",
        {192, 168, 1, 1},
        8001,
        "12345678901234567890",
        "12345678901234567890",
        0,
        0,
        0,
        event: :started,
        trackerid: "this is my tracker id"
      )
  end
end
