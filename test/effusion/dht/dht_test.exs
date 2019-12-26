defmodule Effusion.DHTTest do
  use ExUnit.Case
  alias Effusion.DHT
  doctest Effusion.DHT

  test "node_id" do
    id = DHT.node_id()

    assert is_binary(id)
    assert byte_size(id) == 20
  end

  test "transaction_id" do
    id = DHT.transaction_id()

    assert is_binary(id)
    assert List.ascii_printable?(String.to_charlist(id))
    assert byte_size(id) == 2
  end

  test "token" do
    token = DHT.token()

    assert is_binary(token)
    assert List.ascii_printable?(String.to_charlist(token))
    assert byte_size(token) == 8
  end
end
