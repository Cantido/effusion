defmodule Effusion.Statistics.NetTest do
  use ExUnit.Case
  alias Effusion.Statistics.Net

  test "get and read sent_payload_bytes" do
    assert is_integer Net.add_sent_payload_bytes(1)
    assert is_integer Net.sent_payload_bytes()
  end
end
