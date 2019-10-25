defmodule Effusion.BTP.PeerTest do
  use ExUnit.Case, async: true
  alias Effusion.BTP.Peer
  doctest Effusion.BTP.Peer

  def new do
    Peer.new(
      {{192, 168, 1, 1}, 8000},
      "Fake Peer Id ~~~~~~~",
      "Fake Info Hash ~~~~~"
    )
  end

  test "expresses interest and unchokes after we send a bitfield" do
    peer = new()

    {peer, msgs} = Peer.recv(peer, {:bitfield, <<0>>})

    assert :interested in msgs
    assert :unchoke in msgs

    assert peer.am_interested
    refute peer.am_choking
  end
end
