defmodule Effusion.BTP.SessionTest do
  use ExUnit.Case
  alias Effusion.BTP.Session
  alias Effusion.BTP.Metainfo
  alias Effusion.BTP.Peer
  doctest Effusion.BTP.Session

  def new do
    {:ok, metainfo} = File.read "test/linuxmint-18.3-cinnamon-64bit.iso.torrent"

    {:ok, decode_result} = Metainfo.decode(metainfo)

    peer = Peer.new(
      {{192, 168, 1, 2}, 8000},
      "Fake Peer Id ~~~~~~~",
      "Fake Info Hash ~~~~~",
      self()
    )

    Session.new(decode_result, {{192, 168, 1, 1}, 8080})
    |> Session.add_connected_peer(peer)
  end

  test "expresses interest and unchokes after we send a bitfield" do
    session = new()

    {session, msgs} = Session.recv(session, "Fake Peer Id ~~~~~~~", {:bitfield, <<0>>})


    assert msgs == %{"Fake Peer Id ~~~~~~~" => [:interested, :unchoke]}

    peer = Map.get(session.connected_peers, "Fake Peer Id ~~~~~~~")

    assert peer.am_interested
    refute peer.am_choking
  end
end
