alias Effusion.PWP.Peer

defmodule Effusion.PWP.PeerTest do
  use ExUnit.Case
  doctest Effusion.PWP.Peer
  @moduletag :capture_log

  @local_peer_id <<0 :: size(160)>>
  @remote_peer_id <<19 :: size(160)>>
  @info_hash <<4 :: size(160)>>

  test "responds to handshake" do
    {:ok, _pid} = Registry.register(Effusion.TorrentRegistry, @info_hash, :ok)

    {:ok, {actual_peer_id, actual_info_hash, actual_reserved}, new_state} =
      Peer.handle_handshake({@remote_peer_id, @info_hash, <<0 :: 64>>}, %{})

    assert actual_peer_id == @local_peer_id
    assert actual_info_hash == @info_hash
    assert actual_reserved == <<0 :: 64>>
    assert new_state == %{remote_peer_id: @remote_peer_id}
  end

  test "returns error when info hash isn't recognized" do
    {msg, reason} = Peer.handle_handshake({@remote_peer_id, @info_hash, <<0 :: 64>>}, %{})

    assert msg == :error
    assert reason == :unknown_info_hash
  end

  test "returns error when peer ID matches our own" do
    {:ok, _pid} = Registry.register(Effusion.TorrentRegistry, @info_hash, :ok)
    {msg, reason} = Peer.handle_handshake({@local_peer_id, @info_hash, <<0 :: 64>>}, %{})

    assert msg == :error
    assert reason == :remote_same_as_local
  end
end
