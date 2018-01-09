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

  test "starting state" do
    state = Peer.init()

    assert state.interested == false
    assert state.choked == true
    assert state.have == Effusion.IntSet.new()
    assert state.blocks == MapSet.new()
    assert state.requested == MapSet.new()
  end

  test "chokes when gets a choke message" do
    {:ok, %{choked: true}} = Peer.handle_msg({:choke}, %{choked: false})
  end

  test "unchokes when gets an unchoke message" do
    {:ok, %{choked: false}} = Peer.handle_msg({:unchoke}, %{choked: true})
  end

  test "interested message" do
    {:ok, %{interested: true}} = Peer.handle_msg({:interested}, %{interested: false})
  end

  test "uninterested message" do
    {:ok, %{interested: false}} = Peer.handle_msg({:uninterested}, %{interested: true})
  end

  test "have message" do
    {:ok, %{have: 1}} = Peer.handle_msg({:have, 0}, %{have: 0})
  end

  test "bitfield message" do
    {:ok, %{have: 241}} = Peer.handle_msg({:bitfield, <<0b11110001>>}, %{have: 0})
  end

  test "request message" do
    block = %{index: 4201, offset: 69, size: 12}
    msg = {:request, block}
    start_state = %{requested: MapSet.new}

    expected_requested = MapSet.new([block])

    {:ok, %{requested: actual_requested}} = Peer.handle_msg(msg, start_state)

    assert actual_requested == expected_requested
  end

  test "piece message" do
    block = %{index: 4201, offset: 69, data: <<1, 2, 3, 4, 5>>}
    msg = {:piece, block}
    start_state = %{blocks: MapSet.new}

    expected_blocks = MapSet.new([block])

    {:ok, %{blocks: actual_blocks}} = Peer.handle_msg(msg, start_state)

    assert actual_blocks == expected_blocks
  end

  test "cancel message" do
    block = %{index: 4201, offset: 69, size: 12}
    msg = {:cancel, block}
    start_state = %{requested: MapSet.new([block])}

    expected_requested = MapSet.new()

    {:ok, %{requested: actual_requested}} = Peer.handle_msg(msg, start_state)

    assert actual_requested == expected_requested
  end
end
