defmodule Effusion.PWP.ConnectionTest do
  use ExUnit.Case
  alias Effusion.PWP.Connection
  alias Effusion.BTP.Peer
  alias Effusion.PWP.Socket

  doctest Effusion.PWP.Connection

  @torrent TestHelper.tiny_meta()

  @remote_peer Peer.new(
                 {{127, 0, 0, 1}, 8001},
                 "Other peer 123456789",
                 @torrent.info_hash,
                 self()
               )

  setup do
    {_host, port} = @remote_peer.address
    {:ok, lsock} = Socket.listen(port)

    on_exit(fn ->
      :ok = Socket.close(lsock)
    end)

    %{lsock: lsock}
  end

  test "replies stop when decode of a packet fails" do
    state = {nil, nil, nil, nil}

    actual_response = Connection.handle_packet(nil, <<"bad message!!!">>, state)
    expected_response = {:stop, {:bad_message, :invalid, <<"bad message!!!">>}, state}

    assert actual_response == expected_response
  end

  test "registers with ConnectionRegistry on successful handshake", %{lsock: lsock} do
    {:ok, cpid} = start_supervised({Connection, @remote_peer})

    {:ok, _sock, _remote_peer} = Socket.accept(lsock, @remote_peer)

    :timer.sleep(10)

    connections = Registry.lookup(ConnectionRegistry, @torrent.info_hash)

    assert connections == [{cpid, "Other peer 123456789"}]
  end
end
