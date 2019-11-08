defmodule Effusion.BTP.PiecePickerTest do
  use ExUnit.Case, async: true
  alias Effusion.BTP.PiecePicker
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Block
  alias Effusion.BTP.Peer
  doctest Effusion.BTP.PiecePicker

  @meta TestHelper.tiny_meta()
  @info_hash @meta.info_hash

  setup do
    Effusion.BTP.Metainfo.Directory.insert(@meta)
  end

  test "set of possible requests is empty when we have no peers" do
    torrent = Pieces.new(@info_hash)
    peers = []
    block_size = 1

    possible = PiecePicker.blocks_available(torrent, peers, block_size)

    assert Enum.empty?(possible)
  end

  test "set of possible requests contains all pieces a single if a single peer has them" do
    torrent = Pieces.new(@info_hash)

    peer =
      Peer.new({nil, nil}, "12345678901234567890", "12345678901234567890")
      |> Peer.set_remote_peer_id("Remote peer ID ~~~~~")

    {peer, _} = Peer.recv(peer, {:have, 0})
    {peer, _} = Peer.recv(peer, {:have, 1})
    peers = [peer]
    block_size = 1

    possible = PiecePicker.blocks_available(torrent, peers, block_size)

    # assert possible == []

    # t
    assert Enum.member?(possible, {"Remote peer ID ~~~~~", %Block{index: 0, offset: 0, size: 1}})
    # i
    assert Enum.member?(possible, {"Remote peer ID ~~~~~", %Block{index: 0, offset: 1, size: 1}})
    # n
    assert Enum.member?(possible, {"Remote peer ID ~~~~~", %Block{index: 0, offset: 2, size: 1}})
    # y
    assert Enum.member?(possible, {"Remote peer ID ~~~~~", %Block{index: 1, offset: 0, size: 1}})
    # \n
    assert Enum.member?(possible, {"Remote peer ID ~~~~~", %Block{index: 1, offset: 1, size: 1}})
  end

  test "doesn't act wierd with a huge block size" do
    torrent = Pieces.new(@info_hash)

    peer =
      Peer.new({nil, nil}, "12345678901234567890", "12345678901234567890")
      |> Peer.set_remote_peer_id("Remote peer ID ~~~~~")

    {peer, _} = Peer.recv(peer, {:have, 0})
    {peer, _} = Peer.recv(peer, {:have, 1})
    peers = [peer]
    block_size = 16_384

    possible = PiecePicker.blocks_available(torrent, peers, block_size)

    # tin
    assert Enum.member?(possible, {"Remote peer ID ~~~~~", %Block{index: 0, offset: 0, size: 3}})
    # y\n
    assert Enum.member?(possible, {"Remote peer ID ~~~~~", %Block{index: 1, offset: 0, size: 2}})
  end
end
