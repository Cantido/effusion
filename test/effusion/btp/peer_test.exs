defmodule Effusion.BTP.PeerTest do
  use ExUnit.Case
  alias Effusion.BTP.Peer
  doctest Effusion.BTP.Peer

  def new do
    Peer.new(
      {{192, 168, 1, 1}, 8000},
      "Fake Peer Id ~~~~~~~",
      "Fake Info Hash ~~~~~",
      self()
    )

  end

  test "getting the handshake for a peer" do
    hs = Peer.get_handshake new()

    assert hs == {
      :handshake,
      "Fake Peer Id ~~~~~~~",
      "Fake Info Hash ~~~~~",
    }
  end

  test "check a valid handshake" do
    hs = {
      :handshake,
      "Another Peer Id ~~~~",
      "Fake Info Hash ~~~~~",
      <<0, 0, 0, 0, 0, 0, 0, 0>>
    }

    assert {:ok, peer} = Peer.handshake(new(), hs)

    assert peer.handshaken
    assert peer.remote_peer_id == "Another Peer Id ~~~~"
  end

  test "check an invalid handshake" do
    hs = {
      :handshake,
      "Another Peer Id ~~~~",
      "Bad Info Hash ~~~~~~",
      <<0, 0, 0, 0, 0, 0, 0, 0>>
    }

    assert Peer.handshake(new(), hs) == {
      :error,
      :mismatched_info_hash,
      [expected: "Fake Info Hash ~~~~~", actual: "Bad Info Hash ~~~~~~"]
    }

  end

  test "check a valid handshake when peer has already handshaken" do
    hs = {
      :handshake,
      "Another Peer Id ~~~~",
      "Fake Info Hash ~~~~~",
      <<0, 0, 0, 0, 0, 0, 0, 0>>
    }

    {:ok, peer} = Peer.handshake(new(), hs)

    assert Peer.handshake(peer, hs) == {
      :error,
      :local_peer_already_handshaken
    }
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