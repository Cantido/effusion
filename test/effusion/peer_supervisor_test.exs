defmodule Effusion.PeerSupervisorTest do
  use ExUnit.Case
  doctest Effusion.PeerSupervisor

  @peer_id <<0 :: size(160)>>

  test "spawns peers" do
    assert {:ok, _peer} = Effusion.PeerSupervisor.new_peer(@peer_id)
  end
end
