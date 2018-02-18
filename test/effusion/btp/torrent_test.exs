defmodule Effusion.BTP.TorrentTest do
  use ExUnit.Case
  doctest Effusion.BTP.Torrent
  alias Effusion.BTP.Torrent

  @meta TestHelper.tiny_meta()

  test "keeps track of blocks" do
    torrent = Torrent.new(@meta.info)
    |> Torrent.add_block(%{index: 0, offset: 0, data: "t"})

    assert %{index: 0, offset: 0, data: "t"} in Torrent.blocks(torrent)
  end

  test "compacts blocks" do
    torrent = Torrent.new(@meta.info)
    |> Torrent.add_block(%{index: 0, offset: 0, data: "t"})
    |> Torrent.add_block(%{index: 0, offset: 1, data: "i"})

    assert %{index: 0, offset: 0, data: "ti"} in Torrent.blocks(torrent)
  end

  test "compacts blocks even if they are out of order" do
    torrent = Torrent.new(@meta.info)
    |> Torrent.add_block(%{index: 0, offset: 1, data: "i"})
    |> Torrent.add_block(%{index: 0, offset: 0, data: "t"})

    assert %{index: 0, offset: 0, data: "ti"} in Torrent.blocks(torrent)
  end

  test "verifies complete pieces once we have enough blocks" do
    torrent = Torrent.new(@meta.info)
    |> Torrent.add_block(%{index: 0, offset: 0, data: "t"})
    |> Torrent.add_block(%{index: 0, offset: 1, data: "i"})
    |> Torrent.add_block(%{index: 0, offset: 2, data: "n"})

    assert %{index: 0, data: "tin"} in Torrent.pieces(torrent)
  end

  test "verifies shorter ending pieces once we have enough blocks" do
    torrent = Torrent.new(@meta.info)
    |> Torrent.add_block(%{index: 1, offset: 0, data: "y"})
    |> Torrent.add_block(%{index: 1, offset: 1, data: "\n"})

    assert %{index: 1, data: "y\n"} in Torrent.pieces(torrent)
  end

  test "finished pieces get accumulated" do
    torrent = Torrent.new(@meta.info)
    |> Torrent.add_block(%{index: 0, offset: 0, data: "t"})
    |> Torrent.add_block(%{index: 0, offset: 1, data: "i"})
    |> Torrent.add_block(%{index: 0, offset: 2, data: "n"})
    |> Torrent.add_block(%{index: 1, offset: 0, data: "y"})
    |> Torrent.add_block(%{index: 1, offset: 1, data: "\n"})

    assert %{index: 0, data: "tin"} in Torrent.pieces(torrent)
    assert %{index: 1, data: "y\n"} in Torrent.pieces(torrent)
  end

  test "when a piece fails to verify, the piece is removed" do
    torrent = Torrent.new(@meta.info)
    |> Torrent.add_block(%{index: 0, offset: 0, data: "t"})
    |> Torrent.add_block(%{index: 0, offset: 1, data: "i"})
    |> Torrent.add_block(%{index: 0, offset: 2, data: "x"})

    assert Enum.count(Torrent.pieces(torrent)) == 0
    assert Enum.count(Torrent.blocks(torrent)) == 0
  end
end
