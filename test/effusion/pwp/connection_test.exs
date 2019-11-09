defmodule Effusion.PWP.OutgoingHandlerTest do
  use ExUnit.Case
  alias Effusion.PWP.OutgoingHandler
  alias Effusion.BTP.Peer
  alias Effusion.PWP.Socket

  doctest Effusion.PWP.OutgoingHandler

  @torrent TestHelper.tiny_meta()
  @local_peer_id "Other peer 123456789"

  @remote_peer Peer.new({{127, 0, 0, 1}, 8002})

  setup do
    {_host, port} = @remote_peer.address
    {:ok, lsock} = Socket.listen(port)

    on_exit(fn ->
      :ok = Socket.close(lsock)
    end)

    %{lsock: lsock}
  end

  test "registers with ConnectionRegistry on successful handshake", %{lsock: lsock} do
    address = @remote_peer.address
    local_info_hash = @torrent.info_hash
    local_peer_id = @local_peer_id
    expected_peer_id = @remote_peer.remote_peer_id

    {:ok, cpid} = start_supervised({OutgoingHandler, {address, local_info_hash, local_peer_id, expected_peer_id}})

    {:ok, _sock, _remote_peer} =
      Socket.accept(
        lsock,
        @torrent.info_hash,
        @local_peer_id,
        @remote_peer.remote_peer_id
      )

    :timer.sleep(10)

    connections = Registry.lookup(ConnectionRegistry, @torrent.info_hash)

    assert Enum.member?(connections, {cpid, "Other peer 123456789"})
  end
end
