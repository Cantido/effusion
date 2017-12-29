defmodule Effusion.PeerTest do
  use ExUnit.Case
  doctest Effusion.Peer
  @moduletag :capture_log

  @peer_id <<0 :: size(120)>>

  setup do
    {:ok, peer} = start_supervised {Effusion.Peer, @peer_id}
    %{peer: peer}
  end

  test "get ID", %{peer: peer} do
    assert Effusion.Peer.id(peer) == @peer_id
  end

  test "are temporary workers" do
    assert Supervisor.child_spec(Effusion.Peer, []).restart == :temporary
  end

  test "registers itself in the peer registry" do
    name = {:via, Registry, {Effusion.PeerRegistry, @peer_id}}
    assert Effusion.Peer.id(name) == @peer_id
  end
end
