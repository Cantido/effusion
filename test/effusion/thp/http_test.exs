defmodule Effusion.THP.HTTPTest do
  use ExUnit.Case
  alias Effusion.THP.HTTP
  doctest Effusion.THP.HTTP

  @body File.read! "test/tracker_response.txt"

  test "decodes" do
    {:ok, body} = HTTP.decode @body

    assert Map.has_key?(body, :interval)
    assert Map.has_key?(body, :peers)

    [peer | _rest] = body.peers

    assert peer != nil


    assert Map.has_key?(peer, :peer_id)

    assert Map.has_key?(peer, :port)
    assert is_integer(peer.port)

    assert Map.has_key?(peer, :ip)
    assert {a, b, c, d} = peer.ip
    assert is_integer(a)
    assert is_integer(b)
    assert is_integer(c)
    assert is_integer(d)
  end

end
