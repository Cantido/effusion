defmodule Effusion.TorrentTest do
  use ExUnit.Case, async: true
  alias Effusion.Torrent
  doctest Effusion.Torrent

  describe "get_bitfield/1" do
    test "with tiny torrent" do
      torrent = %Torrent{meta: TestHelper.tiny_meta(), block_size: 3}

      assert <<0>> == Torrent.get_bitfield(torrent)
    end

    test "with large torrent" do
      meta = TestHelper.mint_meta()
      piece_count = Enum.count(meta.info.pieces)
      expected_bit_size = piece_count + rem(piece_count, 8)

      torrent = %Torrent{meta: meta, block_size: 1}

      assert bit_size(Torrent.get_bitfield(torrent)) == expected_bit_size
    end

    test "returns a bit for when a piece is verified" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta(), block_size: 3}
        |> Torrent.piece_verified(0)

      assert <<1::1, 0::7>> == Torrent.get_bitfield(torrent)
    end
  end

  describe "block_written?/3" do
    test "returns false if no blocks have been written" do
      torrent = %Torrent{meta: TestHelper.tiny_meta(), block_size: 3}

      refute Torrent.block_written?(torrent, 0, 0)
      refute Torrent.block_written?(torrent, 1, 0)
    end

    test "returns true if a block was written" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta(), block_size: 3}
        |> Torrent.block_written(0, 0)

      assert Torrent.block_written?(torrent, 0, 0)
      refute Torrent.block_written?(torrent, 1, 0)
    end

    test "written blocks are cleared if the piece fails verification" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta(), block_size: 3}
        |> Torrent.block_written(0, 0)
        |> Torrent.block_written(1, 0)
        |> Torrent.piece_failed_verification(0)

      refute Torrent.block_written?(torrent, 0, 0)
      assert Torrent.block_written?(torrent, 1, 0)
    end

    test "written blocks are cleared if the piece fails verification and blocks are bigger than pieces" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta(), block_size: 16384}
        |> Torrent.block_written(0, 0)
        |> Torrent.block_written(1, 0)
        |> Torrent.piece_failed_verification(0)

      refute Torrent.block_written?(torrent, 0, 0)
      assert Torrent.block_written?(torrent, 1, 0)
    end
  end

  describe "piece_written?/2" do
    test "returns false when no pieces have been written" do
      torrent = %Torrent{meta: TestHelper.tiny_meta(), block_size: 3}

      refute Torrent.piece_written?(torrent, 0)
      refute Torrent.piece_written?(torrent, 1)
    end

    test "returns false when only some of the blocks are written" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta(), block_size: 1}
        |> Torrent.block_written(0, 0)
        |> Torrent.block_written(0, 1)

      refute Torrent.piece_written?(torrent, 0)
      refute Torrent.piece_written?(torrent, 1)
    end

    test "returns true when all of the blocks in the piece have been written" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta(), block_size: 1}
        |> Torrent.block_written(0, 0)
        |> Torrent.block_written(0, 1)
        |> Torrent.block_written(0, 2)

      assert Torrent.piece_written?(torrent, 0)
      refute Torrent.piece_written?(torrent, 1)
    end

    test "returns true when all of the blocks in the piece have been written and blocks are bigger than pieces" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta()}
        |> Torrent.block_written(0, 0)
        |> Torrent.block_written(0, 1)
        |> Torrent.block_written(0, 2)

      assert Torrent.piece_written?(torrent, 0)
      refute Torrent.piece_written?(torrent, 1)
    end

    test "returns true when all of the blocks in the piece have been written and blocks are shorter than nominal" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta()}
        |> Torrent.block_written(1, 0)
        |> Torrent.block_written(1, 1)

      refute Torrent.piece_written?(torrent, 0)
      assert Torrent.piece_written?(torrent, 1)
    end
  end

  describe "piece_verified?/2" do
    test "returns false when no pieces have been verified" do
      torrent = %Torrent{meta: TestHelper.tiny_meta()}

      refute Torrent.piece_verified?(torrent, 0)
      refute Torrent.piece_verified?(torrent, 1)
    end

    test "returns true after piece_verified/2 was called" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta()}
        |> Torrent.piece_verified(0)

      assert Torrent.piece_verified?(torrent, 0)
      refute Torrent.piece_verified?(torrent, 1)
    end
  end

  describe "block_requests/2" do
    test "returns requests for blocks the peer has" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta(), block_size: 3}
        |> Torrent.peer_has_piece({127, 0, 0, 1}, 0)

      actual_requests = Torrent.block_requests(torrent, {127, 0, 0, 1})

      assert Enum.count(actual_requests) == 1
      actual_request = Enum.at(actual_requests, 0)

      assert actual_request == {0, 0, 3}
    end

    test "return value does not include a block if we already have it" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta(), block_size: 3}
        |> Torrent.peer_has_piece({127, 0, 0, 1}, 0)
        |> Torrent.piece_verified(0)

      actual_requests = Torrent.block_requests(torrent, {127, 0, 0, 1})

      assert Enum.empty?(actual_requests)
    end

    test "return value includes parts of pieces" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta(), block_size: 1}
        |> Torrent.peer_has_piece({127, 0, 0, 1}, 0)
        |> Torrent.block_written(0, 0)

      actual_requests = Torrent.block_requests(torrent, {127, 0, 0, 1})

      assert Enum.count(actual_requests) == 2

      assert {0, 1, 1} in actual_requests
      assert {0, 2, 1} in actual_requests
    end

    test "truncates request size to piece size if block is larger" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta(), block_size: 16384}
        |> Torrent.peer_has_piece({127, 0, 0, 1}, 0)
        |> Torrent.peer_has_piece({127, 0, 0, 1}, 1)

      actual_requests = Torrent.block_requests(torrent, {127, 0, 0, 1})

      assert {0, 0, 3} in actual_requests
      assert {1, 0, 2} in actual_requests

      assert Enum.count(actual_requests) == 2
    end
  end
end
