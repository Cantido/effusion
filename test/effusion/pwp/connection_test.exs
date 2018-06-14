defmodule Effusion.PWP.ConnectionTest do
  use ExUnit.Case
  alias Effusion.PWP.Connection
  doctest Effusion.PWP.Connection

  test "replies stop when decode of a packet fails" do
    state = {nil, nil, nil}

    actual_response = Connection.handle_packet(nil, <<"bad message!!!">>, state)
    expected_response = {:stop, {:bad_message, :invalid, <<"bad message!!!">>}, state}

    assert actual_response == expected_response
  end
end
