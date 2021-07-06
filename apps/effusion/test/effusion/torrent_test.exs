defmodule Effusion.TorrentTest do
  use ExUnit.Case
  alias Effusion.{Torrent, Piece}
  doctest Effusion.Torrent

  describe "get_bitfield/1" do
    test "with tiny torrent" do
      torrent = %Torrent{meta: TestHelper.tiny_meta()}

      assert <<0>> == Torrent.get_bitfield(torrent)
    end

    test "with large torrent" do
      meta = TestHelper.mint_meta()
      piece_count = Enum.count(meta.info.pieces)
      expected_bit_size = piece_count + rem(piece_count, 8)

      torrent = %Torrent{meta: meta}

      assert bit_size(Torrent.get_bitfield(torrent)) == expected_bit_size
    end

    test "when a piece is verified" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta()}
        |> Torrent.add_data(0, 0, "tin")

      assert <<1::1, 0::7>> == Torrent.get_bitfield(torrent)
    end

    test "when a piece is written" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta()}
        |> Torrent.piece_written(0)

      assert <<1::1, 0::7>> == Torrent.get_bitfield(torrent)
    end
  end

  describe "add_data/4" do
    test "stores data" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta()}
        |> Torrent.add_data(0, 0, "tin")

      assert Piece.data(Torrent.get_piece(torrent, 0)) == "tin"
    end

    test "increments bytes_downloaded when data is added" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta()}
        |> Torrent.add_data(0, 0, "tin")

      assert torrent.bytes_downloaded == 3
    end
  end

  describe "block_requests/2" do
    test "returns requests for blocks the peer has" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta()}
        |> Torrent.peer_has_piece({127, 0, 0, 1}, 0)

      actual_requests = Torrent.block_requests(torrent, {127, 0, 0, 1})

      assert Enum.count(actual_requests) == 1
      actual_request = Enum.at(actual_requests, 0)

      assert actual_request == {0, 0, 3}
    end

    test "return value does not include a block if we already have it" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta()}
        |> Torrent.peer_has_piece({127, 0, 0, 1}, 0)
        |> Torrent.add_data(0, 0, "tin")

      actual_requests = Torrent.block_requests(torrent, {127, 0, 0, 1})

      assert Enum.empty?(actual_requests)
    end

    test "return value includes parts of pieces" do
      torrent =
        %Torrent{meta: TestHelper.tiny_meta()}
        |> Torrent.peer_has_piece({127, 0, 0, 1}, 0)
        |> Torrent.add_data(0, 0, "t")

      actual_requests = Torrent.block_requests(torrent, {127, 0, 0, 1}, 1)

      assert Enum.count(actual_requests) == 2

      assert {0, 1, 1} in actual_requests
      assert {0, 2, 1} in actual_requests
    end
  end
end
