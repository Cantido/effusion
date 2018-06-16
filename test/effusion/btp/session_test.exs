defmodule Effusion.BTP.SessionTest do
  use ExUnit.Case, async: true
  alias Effusion.BTP.Session
  alias Effusion.BTP.Peer
  doctest Effusion.BTP.Session
  import Mox

  setup :verify_on_exit!



  def new do
    metainfo = TestHelper.tiny_meta()

    peer = Peer.new(
      {{192, 168, 1, 2}, 8000},
      "Fake Peer Id ~~~~~~~",
      "Fake Info Hash ~~~~~",
      self()
    )

    Session.new(metainfo, {{192, 168, 1, 1}, 8080})
    |> Session.add_connected_peer(peer)
  end

  test "expresses interest and unchokes after we send a bitfield" do
    session = new()

    {session, msgs} = Session.handle_message(session, "Fake Peer Id ~~~~~~~", {:bitfield, <<0>>})


    assert msgs == [:interested, :unchoke]

    peer = Map.get(session.connected_peers, "Fake Peer Id ~~~~~~~")

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

    {session, _msg} = new()
    |> Session.handle_message("Fake Peer Id ~~~~~~~", {:piece, %{index: 0, offset: 0, data: "tin"}})

    Session.announce(session, Effusion.THP.Mock)
  end
end
