defmodule Effusion.BTP.PieceSelectionTest do
  use ExUnit.Case, async: true
  alias Effusion.BTP.PieceSelection
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.Peer
  doctest Effusion.BTP.PieceSelection

  @meta TestHelper.tiny_meta()
  @info_hash @meta.info_hash

  setup do
    :ets.insert(MetadataTable, {@info_hash, @meta})
    :ok
  end

  test "set of possible requests is empty when we have no peers" do
    torrent = Torrent.new(@info_hash)
    peers = []
    block_size = 1

    possible = PieceSelection.possible_requests(torrent, peers, block_size)

    assert Enum.empty?(possible)
  end

  test "set of possible requests contains all pieces a single if a single peer has them" do
    torrent = Torrent.new(@info_hash)
    peer = Peer.new(
      {nil, nil},
      "12345678901234567890",
      "12345678901234567890",
      self())
    |> Peer.set_remote_peer_id("Remote peer ID ~~~~~")
    {peer, _} = Peer.recv(peer, {:have, 0})
    {peer, _} = Peer.recv(peer, {:have, 1})
    peers = [peer]
    block_size = 1

    possible = PieceSelection.possible_requests(torrent, peers, block_size)

    # assert possible == []

    assert Enum.member?(possible, {"Remote peer ID ~~~~~", %{index: 0, offset: 0, size: 1}}) # t
    assert Enum.member?(possible, {"Remote peer ID ~~~~~", %{index: 0, offset: 1, size: 1}}) # i
    assert Enum.member?(possible, {"Remote peer ID ~~~~~", %{index: 0, offset: 2, size: 1}}) # n
    assert Enum.member?(possible, {"Remote peer ID ~~~~~", %{index: 1, offset: 0, size: 1}}) # y
    assert Enum.member?(possible, {"Remote peer ID ~~~~~", %{index: 1, offset: 1, size: 1}}) # \n
  end

  test "doesn't act wierd with a huge block size" do
    torrent = Torrent.new(@info_hash)
    peer = Peer.new(
      {nil, nil},
      "12345678901234567890",
      "12345678901234567890",
      self())
    |> Peer.set_remote_peer_id("Remote peer ID ~~~~~")
    {peer, _} = Peer.recv(peer, {:have, 0})
    {peer, _} = Peer.recv(peer, {:have, 1})
    peers = [peer]
    block_size = 16_384

    possible = PieceSelection.possible_requests(torrent, peers, block_size)

    # assert possible == []

    assert Enum.member?(possible, {"Remote peer ID ~~~~~", %{index: 0, offset: 0, size: 3}}) # tin
    assert Enum.member?(possible, {"Remote peer ID ~~~~~", %{index: 1, offset: 0, size: 2}}) # y\n
  end
end
