defmodule Effusion.BTP.PiecesTest do
  use ExUnit.Case, async: true
  doctest Effusion.BTP.Pieces
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Metainfo
  alias Effusion.BTP.Block

  @meta TestHelper.tiny_meta()

  setup do
    Effusion.BTP.Metainfo.Directory.insert(@meta)
  end

  # test "keeps track of blocks" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.add_block(%Block{index: 0, offset: 0, data: "t", size: 1})
  #
  #   assert %Block{index: 0, offset: 0, data: "t", size: 1} in Pieces.unfinished(torrent)
  # end
  #
  # test "compacts blocks" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.add_block(%Block{index: 0, offset: 0, data: "t", size: 1})
  #     |> Pieces.add_block(%Block{index: 0, offset: 1, data: "i", size: 1})
  #
  #   assert %Block{index: 0, offset: 0, data: "ti", size: 2} in Pieces.unfinished(torrent)
  # end
  #
  # test "compacts blocks even if they are out of order" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.add_block(%Block{index: 0, offset: 1, data: "i", size: 1})
  #     |> Pieces.add_block(%Block{index: 0, offset: 0, data: "t", size: 1})
  #
  #   assert %Block{index: 0, offset: 0, data: "ti", size: 2} in Pieces.unfinished(torrent)
  # end
  #
  # test "verifies complete pieces once we have enough blocks" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.add_block(%Block{index: 0, offset: 0, data: "t", size: 1})
  #     |> Pieces.add_block(%Block{index: 0, offset: 1, data: "i", size: 1})
  #     |> Pieces.add_block(%Block{index: 0, offset: 2, data: "n", size: 1})
  #
  #   assert %{index: 0, data: "tin"} in Pieces.verified(torrent)
  #   assert Pieces.unfinished(torrent) |> Enum.empty?()
  # end
  #
  # test "verifies shorter ending pieces once we have enough blocks" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.add_block(%{index: 1, offset: 0, data: "y", size: 1})
  #     |> Pieces.add_block(%{index: 1, offset: 1, data: "\n", size: 1})
  #
  #   assert %{index: 1, data: "y\n"} in Pieces.verified(torrent)
  # end
  #
  # test "finished pieces get accumulated" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.add_block(%{index: 0, offset: 0, data: "t", size: 1})
  #     |> Pieces.add_block(%{index: 0, offset: 1, data: "i", size: 1})
  #     |> Pieces.add_block(%{index: 0, offset: 2, data: "n", size: 1})
  #     |> Pieces.add_block(%{index: 1, offset: 0, data: "y", size: 1})
  #     |> Pieces.add_block(%{index: 1, offset: 1, data: "\n", size: 1})
  #
  #   assert %{index: 0, data: "tin"} in Pieces.verified(torrent)
  #   assert %{index: 1, data: "y\n"} in Pieces.verified(torrent)
  # end
  #
  # test "when a piece fails to verify, the piece is removed" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.add_block(%{index: 0, offset: 0, data: "t", size: 1})
  #     |> Pieces.add_block(%{index: 0, offset: 1, data: "i", size: 1})
  #     |> Pieces.add_block(%{index: 0, offset: 2, data: "x", size: 1})
  #
  #   assert Enum.count(Pieces.verified(torrent)) == 0
  #   assert Enum.count(Pieces.unfinished(torrent)) == 0
  # end
  #
  # test "add block and take verified" do
  #   torrent = Pieces.new(@meta.info_hash)
  #
  #   {verified, torrent} =
  #     Pieces.add_block_and_take_verified(torrent, %{index: 0, offset: 0, data: "tin", size: 3})
  #
  #   assert %{index: 0, data: "tin"} in verified
  #   assert Pieces.verified(torrent) |> Enum.empty?()
  #   assert Pieces.written(torrent) == IntSet.new(0)
  # end
  #
  # test "take_verified removes verified pieces from torrent" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.add_block(%{index: 0, offset: 0, data: "tin", size: 3})
  #
  #   assert {verified, torrent} = Pieces.take_verified(torrent)
  #   assert %{index: 0, data: "tin"} in verified
  #   assert Pieces.verified(torrent) |> Enum.empty?()
  #   assert Pieces.written(torrent) == IntSet.new(0)
  # end
  #
  # test "all_present? is false when the part of the file is provided" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.add_block(%{index: 0, offset: 0, data: "tin", size: 3})
  #
  #   refute Pieces.all_present?(torrent)
  # end
  #
  # test "all_present? is true when the entire file is provided" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.add_block(%{index: 0, offset: 0, data: "tin", size: 3})
  #     |> Pieces.add_block(%{index: 1, offset: 0, data: "y\n", size: 2})
  #
  #   assert Pieces.all_present?(torrent)
  # end
  #
  # test "count of pieces in-memory and written stays consistent" do
  #   torrent = Pieces.new(@meta.info_hash)
  #
  #   refute Pieces.all_present?(torrent)
  #   assert Pieces.unfinished(torrent) |> Enum.empty?()
  #   assert Pieces.verified(torrent) |> Enum.empty?()
  #   assert Pieces.written(torrent) |> Enum.empty?()
  #   assert Pieces.bytes_completed(torrent) == 0
  #   assert Pieces.bytes_left(torrent) == 5
  #
  #   torrent = Pieces.add_block(torrent, %{index: 0, offset: 0, data: "t", size: 1})
  #
  #   assert Pieces.unfinished(torrent) == MapSet.new([%{index: 0, offset: 0, data: "t", size: 1}])
  #   assert Pieces.verified(torrent) |> Enum.empty?()
  #   assert Pieces.written(torrent) |> Enum.empty?()
  #   assert Pieces.bytes_completed(torrent) == 1
  #   assert Pieces.bytes_left(torrent) == 4
  #
  #   torrent = Pieces.add_block(torrent, %Block{index: 0, offset: 1, data: "in", size: 2})
  #
  #   assert Pieces.unfinished(torrent) |> Enum.empty?()
  #   assert Pieces.verified(torrent) == MapSet.new([%{index: 0, data: "tin"}])
  #   assert Pieces.written(torrent) |> Enum.empty?()
  #   assert Pieces.bytes_completed(torrent) == 3
  #   assert Pieces.bytes_left(torrent) == 2
  #
  #   torrent = Pieces.mark_piece_written(torrent, %{index: 0})
  #
  #   assert Pieces.unfinished(torrent) |> Enum.empty?()
  #   assert Pieces.verified(torrent) |> Enum.empty?()
  #   assert Pieces.written(torrent) == IntSet.new(0)
  #   assert Pieces.bytes_completed(torrent) == 3
  #   assert Pieces.bytes_left(torrent) == 2
  # end
  #
  # test "bytes_left returns torrent size if no pieces were added" do
  #   torrent = Pieces.new(@meta.info_hash)
  #
  #   assert Pieces.bytes_left(torrent) == @meta.info.length
  # end
  #
  # test "bytes_completed returns torrent size if the torrent is complete" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.add_block(%{index: 0, offset: 0, data: "tin", size: 3})
  #     |> Pieces.add_block(%{index: 1, offset: 0, data: "y\n", size: 2})
  #
  #   assert Pieces.bytes_completed(torrent) == 5
  # end
  #
  # test "bytes_completed returns a value if the torrent is incomplete" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.add_block(%{index: 0, offset: 0, data: "tin", size: 3})
  #
  #   assert Pieces.bytes_completed(torrent) == 3
  # end
  #
  # test "bytes_completed returns a value if the torrent is incomplete and has unfinished pieces" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.add_block(%{index: 0, offset: 0, data: "tin", size: 3})
  #     |> Pieces.add_block(%{index: 1, offset: 0, data: "y", size: 1})
  #
  #   assert Pieces.bytes_left(torrent) == 1
  # end
  #
  # test "bytes_completed returns a value if the torrent is incomplete, and we have the last odd-sized piece" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.add_block(%{index: 1, offset: 0, data: "y\n", size: 2})
  #
  #   assert Pieces.bytes_completed(torrent) == 2
  # end
  #
  # test "bytes_completed returns a value if some pieces have been written out" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.mark_piece_written(%{index: 0})
  #
  #   assert Pieces.bytes_completed(torrent) == 3
  # end
  #
  # test "bytes_completed returns a value if the very last piece has been written out" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.mark_piece_written(%{index: 1})
  #
  #   assert Pieces.bytes_completed(torrent) == 2
  # end
  #
  # test "bytes_left returns zero if the torrent is complete" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.add_block(%{index: 0, offset: 0, data: "tin", size: 3})
  #     |> Pieces.add_block(%{index: 1, offset: 0, data: "y\n", size: 2})
  #
  #   assert Pieces.bytes_left(torrent) == 0
  # end
  #
  # test "bytes_left returns a value if the torrent is incomplete" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.add_block(%{index: 0, offset: 0, data: "tin", size: 3})
  #
  #   assert Pieces.bytes_left(torrent) == 2
  # end
  #
  # test "bytes_left returns a value if the torrent is incomplete and has unfinished pieces" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.add_block(%{index: 0, offset: 0, data: "t", size: 1})
  #
  #   assert Pieces.bytes_left(torrent) == 4
  # end
  #
  # test "bytes_left returns a value if the torrent is incomplete, and we have the last odd-sized piece" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.add_block(%{index: 1, offset: 0, data: "y\n", size: 2})
  #
  #   assert Pieces.bytes_left(torrent) == 3
  # end
  #
  # test "bytes_left returns a value if some pieces have been written out" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.mark_piece_written(%{index: 0})
  #
  #   assert Pieces.bytes_left(torrent) == 2
  # end
  #
  # test "bytes_left returns a value if the very last piece has been written out" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.mark_piece_written(%{index: 1})
  #
  #   assert Pieces.bytes_left(torrent) == 3
  # end
  #
  # test "can mark many pieces as written" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.mark_pieces_written([%{index: 0}, %{index: 1}])
  #
  #   assert Pieces.bytes_left(torrent) == 0
  #   assert Pieces.bytes_completed(torrent) == 5
  # end
  #
  # test "new torrent has an empty bitfield" do
  #   torrent = Pieces.new(@meta.info_hash)
  #
  #   assert Enum.empty?(Pieces.bitfield(torrent))
  # end
  #
  # test "a torrent with a written piece has that piece in its bitfield" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.mark_piece_written(%{index: 1})
  #
  #   assert Pieces.bitfield(torrent) |> Enum.member?(1)
  # end
  #
  # test "a torrent with a piece in memory has that piece in its bitfield" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.add_block(%{index: 1, offset: 0, data: "y\n", size: 2})
  #
  #   assert Pieces.bitfield(torrent) |> Enum.member?(1)
  # end
  #
  # test "a torrent with a written piece and a cached piece has both in its bitfield" do
  #   torrent =
  #     Pieces.new(@meta.info_hash)
  #     |> Pieces.mark_piece_written(%{index: 0})
  #     |> Pieces.add_block(%{index: 1, offset: 0, data: "y\n", size: 2})
  #
  #   assert 0 in Pieces.bitfield(torrent)
  #   assert 1 in Pieces.bitfield(torrent)
  # end
  #
  # test "accepts blocks for multi-file torrents" do
  #   {:ok, metabin} = File.read("test/hello_world.torrent")
  #   {:ok, meta} = Metainfo.decode(metabin)
  #
  #   torrent =
  #     meta.info_hash
  #     |> Pieces.new()
  #     |> Pieces.add_block(Block.new(0, 0, "Hello\nworld!\n"))
  #
  #   assert Pieces.all_present?(torrent)
  # end
end
