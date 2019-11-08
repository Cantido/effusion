defmodule Effusion.PWP.OutgoingHandlerTest do
  use ExUnit.Case
  alias Effusion.PWP.OutgoingHandler
  alias Effusion.BTP.Peer
  alias Effusion.PWP.Socket

  doctest Effusion.PWP.OutgoingHandler

  @torrent TestHelper.tiny_meta()

  @remote_peer Peer.new(
                 {{127, 0, 0, 1}, 8002},
                 "Other peer 123456789",
                 @torrent.info_hash
               )

  setup do
    {_host, port} = @remote_peer.address
    {:ok, lsock} = Socket.listen(port)

    on_exit(fn ->
      :ok = Socket.close(lsock)
    end)

    %{lsock: lsock}
  end

  test "registers with ConnectionRegistry on successful handshake", %{lsock: lsock} do
    {:ok, cpid} = start_supervised({OutgoingHandler, @remote_peer})

    {:ok, _sock, _remote_peer} = Socket.accept(lsock, @remote_peer.info_hash, @remote_peer.peer_id, @remote_peer.remote_peer_id)

    :timer.sleep(10)

    connections = Registry.lookup(ConnectionRegistry, @torrent.info_hash)

    assert Enum.member? connections, {cpid, "Other peer 123456789"}
  end
end
