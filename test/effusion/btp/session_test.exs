defmodule Effusion.BTP.SessionTest do
  use ExUnit.Case, async: true
  alias Effusion.BTP.Session
  alias Effusion.BTP.Peer
  doctest Effusion.BTP.Session
  import Mox

  setup :verify_on_exit!

  @torrent TestHelper.tiny_meta()

  def peer do
    Peer.new(
      {{192, 168, 1, 2}, 8000},
      "local peer id ~~~~~~",
      @torrent.info_hash,
      self()
    )
    |> Map.put(:remote_peer_id, "Fake Peer Id ~~~~~~~")
  end

  def new do
    Session.new(@torrent, {{192, 168, 1, 1}, 8080})
    |> Session.add_connected_peer(peer())
  end

  test "expresses interest and unchokes after we send a bitfield" do
    peer = peer()
    session = new()

    {session, msgs} = Session.handle_message(session, peer.remote_peer_id, {:bitfield, <<0>>})


    assert msgs == [:interested, :unchoke]

    peer = Map.get(session.connected_peers, peer.remote_peer_id)

    assert peer.am_interested
    refute peer.am_choking
  end

  test "sends torrent's download progress in announce" do
    stub_tracker = fn (_, _, _, _, _, _, downloaded, left) ->
      assert downloaded == 3
      assert left == 2
      {:ok, %{interval: 9_000, peers: []}}
    end

    Effusion.THP.Mock
    |> expect(:announce, stub_tracker)

    peer = peer()

    {session, _msg} = new()
    |> Session.handle_message(peer.remote_peer_id, {:piece, %{index: 0, offset: 0, data: "tin"}})

    Session.announce(session, Effusion.THP.Mock)
  end

  test "sends HAVE messages once it finishes a piece" do
    stub_tracker = fn (_, _, _, _, _, _, _, _) ->
      {:ok, %{interval: 9_000, peers: []}}
    end

    Effusion.THP.Mock
    |> stub(:announce, stub_tracker)

    peer = peer()

    {:ok, _pid} = Registry.register(ConnectionRegistry, peer.info_hash, peer.remote_peer_id)

    {_session, _msg} = new()
    |> Session.handle_message(peer.remote_peer_id, {:piece, %{index: 0, offset: 0, data: "tin"}})

    assert_receive({:btp_send, {:have, 0}})
  end
end
