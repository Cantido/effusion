defmodule Effusion.PWP.ConnectionTest do
  use ExUnit.Case
  alias Effusion.PWP.Connection
  doctest Effusion.PWP.Connection

  test "replies stop when decode of a packet fails" do
    packet = {:tcp, nil, <<"bad message!!!">>}
    state = %{session: nil, peer_id: nil}

    actual_response = Connection.handle_info(packet, state)
    expected_response = {:stop, {:bad_message, :invalid, <<"bad message!!!">>}, state}

    assert actual_response == expected_response
  end
end
