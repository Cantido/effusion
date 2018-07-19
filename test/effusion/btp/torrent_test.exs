defmodule Effusion.BTP.TorrentTest do
  use ExUnit.Case, async: true
  doctest Effusion.BTP.Torrent
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.Metainfo
  alias Effusion.BTP.Block

  @meta TestHelper.tiny_meta()

  test "keeps track of blocks" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.add_block(%{index: 0, offset: 0, data: "t"})

    assert %{index: 0, offset: 0, data: "t"} in Torrent.unfinished(torrent)
  end

  test "compacts blocks" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.add_block(%{index: 0, offset: 0, data: "t"})
    |> Torrent.add_block(%{index: 0, offset: 1, data: "i"})

    assert %{index: 0, offset: 0, data: "ti"} in Torrent.unfinished(torrent)
  end

  test "compacts blocks even if they are out of order" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.add_block(%{index: 0, offset: 1, data: "i"})
    |> Torrent.add_block(%{index: 0, offset: 0, data: "t"})

    assert %{index: 0, offset: 0, data: "ti"} in Torrent.unfinished(torrent)
  end

  test "verifies complete pieces once we have enough blocks" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.add_block(%{index: 0, offset: 0, data: "t"})
    |> Torrent.add_block(%{index: 0, offset: 1, data: "i"})
    |> Torrent.add_block(%{index: 0, offset: 2, data: "n"})

    assert %{index: 0, data: "tin"} in Torrent.verified(torrent)
    assert Torrent.unfinished(torrent) |> Enum.empty?()
  end

  test "verifies shorter ending pieces once we have enough blocks" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.add_block(%{index: 1, offset: 0, data: "y"})
    |> Torrent.add_block(%{index: 1, offset: 1, data: "\n"})

    assert %{index: 1, data: "y\n"} in Torrent.verified(torrent)
  end

  test "finished pieces get accumulated" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.add_block(%{index: 0, offset: 0, data: "t"})
    |> Torrent.add_block(%{index: 0, offset: 1, data: "i"})
    |> Torrent.add_block(%{index: 0, offset: 2, data: "n"})
    |> Torrent.add_block(%{index: 1, offset: 0, data: "y"})
    |> Torrent.add_block(%{index: 1, offset: 1, data: "\n"})

    assert %{index: 0, data: "tin"} in Torrent.verified(torrent)
    assert %{index: 1, data: "y\n"} in Torrent.verified(torrent)
  end

  test "when a piece fails to verify, the piece is removed" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.add_block(%{index: 0, offset: 0, data: "t"})
    |> Torrent.add_block(%{index: 0, offset: 1, data: "i"})
    |> Torrent.add_block(%{index: 0, offset: 2, data: "x"})

    assert Enum.count(Torrent.verified(torrent)) == 0
    assert Enum.count(Torrent.unfinished(torrent)) == 0
  end

  test "add block and take verified" do
    torrent = Torrent.new(@meta.info_hash)

    {verified, torrent} = Torrent.add_block_and_take_verified(torrent, %{index: 0, offset: 0, data: "tin"})

    assert %{index: 0, data: "tin"} in verified
    assert Torrent.verified(torrent) |> Enum.empty?()
    assert Torrent.written(torrent) == IntSet.new(0)
  end

  test "take_verified removes verified pieces from torrent" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.add_block(%{index: 0, offset: 0, data: "tin"})

    assert {verified, torrent} = Torrent.take_verified(torrent)
    assert %{index: 0, data: "tin"} in verified
    assert Torrent.verified(torrent) |> Enum.empty?()
    assert Torrent.written(torrent) == IntSet.new(0)
  end

  test "all_present? is false when the part of the file is provided" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.add_block(%{index: 0, offset: 0, data: "tin"})

    refute Torrent.all_present?(torrent)
  end

  test "all_present? is true when the entire file is provided" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.add_block(%{index: 0, offset: 0, data: "tin"})
    |> Torrent.add_block(%{index: 1, offset: 0, data: "y\n"})

    assert Torrent.all_present?(torrent)
  end

  test "count of pieces in-memory and written stays consistent" do
    torrent = Torrent.new(@meta.info_hash)

    refute Torrent.all_present?(torrent)
    assert Torrent.unfinished(torrent) |> Enum.empty?()
    assert Torrent.verified(torrent) |> Enum.empty?()
    assert Torrent.written(torrent) |> Enum.empty?()
    assert Torrent.bytes_completed(torrent) == 0
    assert Torrent.bytes_left(torrent) == 5

    torrent = Torrent.add_block(torrent, %{index: 0, offset: 0, data: "t"})

    assert Torrent.unfinished(torrent) == MapSet.new([%{index: 0, offset: 0, data: "t"}])
    assert Torrent.verified(torrent) |> Enum.empty?()
    assert Torrent.written(torrent) |> Enum.empty?()
    assert Torrent.bytes_completed(torrent) == 1
    assert Torrent.bytes_left(torrent) == 4

    torrent = Torrent.add_block(torrent, %{index: 0, offset: 1, data: "in"})

    assert Torrent.unfinished(torrent) |> Enum.empty?()
    assert Torrent.verified(torrent) == MapSet.new([%{index: 0, data: "tin"}])
    assert Torrent.written(torrent) |> Enum.empty?()
    assert Torrent.bytes_completed(torrent) == 3
    assert Torrent.bytes_left(torrent) == 2

    torrent = Torrent.mark_piece_written(torrent, %{index: 0})

    assert Torrent.unfinished(torrent) |> Enum.empty?()
    assert Torrent.verified(torrent) |> Enum.empty?()
    assert Torrent.written(torrent) == IntSet.new(0)
    assert Torrent.bytes_completed(torrent) == 3
    assert Torrent.bytes_left(torrent) == 2
  end

  test "bytes_left returns torrent size if no pieces were added" do
    torrent = Torrent.new(@meta.info_hash)

    assert Torrent.bytes_left(torrent) == @meta.info.length
  end

  test "bytes_completed returns torrent size if the torrent is complete" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.add_block(%{index: 0, offset: 0, data: "tin"})
    |> Torrent.add_block(%{index: 1, offset: 0, data: "y\n"})

    assert Torrent.bytes_completed(torrent) == 5
  end

  test "bytes_completed returns a value if the torrent is incomplete" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.add_block(%{index: 0, offset: 0, data: "tin"})

    assert Torrent.bytes_completed(torrent) == 3
  end

  test "bytes_completed returns a value if the torrent is incomplete and has unfinished pieces" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.add_block(%{index: 0, offset: 0, data: "tin"})
    |> Torrent.add_block(%{index: 1, offset: 0, data: "y"})

    assert Torrent.bytes_left(torrent) == 1
  end

  test "bytes_completed returns a value if the torrent is incomplete, and we have the last odd-sized piece" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.add_block(%{index: 1, offset: 0, data: "y\n"})

    assert Torrent.bytes_completed(torrent) == 2
  end

  test "bytes_completed returns a value if some pieces have been written out" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.mark_piece_written(%{index: 0})

    assert Torrent.bytes_completed(torrent) == 3
  end

  test "bytes_completed returns a value if the very last piece has been written out" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.mark_piece_written(%{index: 1})

    assert Torrent.bytes_completed(torrent) == 2
  end

  test "bytes_left returns zero if the torrent is complete" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.add_block(%{index: 0, offset: 0, data: "tin"})
    |> Torrent.add_block(%{index: 1, offset: 0, data: "y\n"})

    assert Torrent.bytes_left(torrent) == 0
  end

  test "bytes_left returns a value if the torrent is incomplete" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.add_block(%{index: 0, offset: 0, data: "tin"})

    assert Torrent.bytes_left(torrent) == 2
  end

  test "bytes_left returns a value if the torrent is incomplete and has unfinished pieces" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.add_block(%{index: 0, offset: 0, data: "t"})

    assert Torrent.bytes_left(torrent) == 4
  end

  test "bytes_left returns a value if the torrent is incomplete, and we have the last odd-sized piece" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.add_block(%{index: 1, offset: 0, data: "y\n"})

    assert Torrent.bytes_left(torrent) == 3
  end

  test "bytes_left returns a value if some pieces have been written out" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.mark_piece_written(%{index: 0})

    assert Torrent.bytes_left(torrent) == 2
  end

  test "bytes_left returns a value if the very last piece has been written out" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.mark_piece_written(%{index: 1})

    assert Torrent.bytes_left(torrent) == 3
  end

  test "can mark many pieces as written" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.mark_pieces_written([%{index: 0}, %{index: 1}])

    assert Torrent.bytes_left(torrent) == 0
    assert Torrent.bytes_completed(torrent) == 5
  end

  test "new torrent has an empty bitfield" do
    torrent = Torrent.new(@meta.info_hash)

    assert Enum.empty?(Torrent.bitfield(torrent))
  end

  test "a torrent with a written piece has that piece in its bitfield" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.mark_piece_written(%{index: 1})

    assert Torrent.bitfield(torrent) |> Enum.member?(1)
  end

  test "a torrent with a piece in memory has that piece in its bitfield" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.add_block(%{index: 1, offset: 0, data: "y\n"})

    assert Torrent.bitfield(torrent) |> Enum.member?(1)
  end

  test "a torrent with a written piece and a cached piece has both in its bitfield" do
    torrent = Torrent.new(@meta.info_hash)
    |> Torrent.mark_piece_written(%{index: 0})
    |> Torrent.add_block(%{index: 1, offset: 0, data: "y\n"})

    assert Torrent.bitfield(torrent) == IntSet.new([0, 1])
  end

  test "accepts blocks for multi-file torrents" do
    {:ok, metabin} = File.read "test/hello_world.torrent"
    {:ok, meta} = Metainfo.decode(metabin)

    torrent = meta.info_hash
    |> Torrent.new()
    |> Torrent.add_block(Block.new(0, 0, "Hello\nworld!\n"))

    assert Torrent.all_present?(torrent)
  end
end
