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

    test "when a piece is verified" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta(), block_size: 3}
        |> Torrent.piece_verified(0)

      assert <<1::1, 0::7>> == Torrent.get_bitfield(torrent)
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
      # This will not happen in real cases but it comes up enough in testing.
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
