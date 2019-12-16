defmodule Effusion.PWP.TCP.ConnectionTest do
  use ExUnit.Case
  alias Effusion.BTP.Peer
  alias Effusion.BTP.Torrent
  alias Effusion.PWP.TCP.OutgoingHandler
  alias Effusion.PWP.TCP.Socket

  doctest Effusion.PWP.TCP.Connection

  @torrent TestHelper.tiny_meta()
  @local_peer_id Application.get_env(:effusion, :peer_id)
  @remote_peer_id "Remote peer 23456789"
  @remote_peer Peer.new({{127, 0, 0, 1}, 8002}, @remote_peer_id)

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Effusion.Repo, { :shared, self() })
  end

  setup do
    {_host, port} = @remote_peer.address
    {:ok, lsock} = Socket.listen(port)

    on_exit(fn ->
      :ok = Socket.close(lsock)
    end)

    %{lsock: lsock}
  end

  setup do
    Torrent.insert(@torrent)
    :ok
  end

  test "registers with ConnectionRegistry on successful handshake", %{lsock: lsock} do
    address = @remote_peer.address
    local_info_hash = @torrent.info_hash
    expected_peer_id = @remote_peer.peer_id

    # OutgoingHandler is acting as our local peer
    {:ok, cpid} = start_supervised({OutgoingHandler, {address, local_info_hash, expected_peer_id}})

    # This socket is acting as a connection from the remote peer
    {:ok, _sock, _remote_peer, _extensions} =
      Socket.accept(
        lsock,
        @torrent.info_hash,
        expected_peer_id,
        @local_peer_id,
        []
      )

    :timer.sleep(50)

    connections = Registry.lookup(ConnectionRegistry, @torrent.info_hash)

    assert Enum.member?(connections, {cpid, @remote_peer_id})
  end
end
