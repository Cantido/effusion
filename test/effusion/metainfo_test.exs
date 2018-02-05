defmodule Effusion.MetainfoTest do
  use ExUnit.Case
  alias Effusion.Metainfo
  doctest Effusion.Metainfo

  test "adds info_hash to decoded file" do
    {:ok, metainfo} = File.read "test/linuxmint-18.3-cinnamon-64bit.iso.torrent"

    {:ok, decode_result} = Metainfo.decode(metainfo)

    assert decode_result != nil
    assert Map.get(decode_result, :info_hash) == TestHelper.mint_info_hash()
  end

  test "breaks info.pieces into a list of 20-byte chunks" do
    {:ok, metainfo} = File.read "test/hello.txt.torrent"

    {:ok, decode_result} = Metainfo.decode(metainfo)

    # the test file is smaller than one piece, so the first piece hash is
    # the hash of the entire file.

    first_piece_hash = :crypto.hash(:sha, "Hello world!\n")
    expected_pieces = [first_piece_hash]

    assert decode_result != nil
    assert get_in(decode_result, [:info, :pieces]) == expected_pieces
  end
end
