defmodule Effusion.BTP.MetainfoTest do
  use ExUnit.Case
  alias Effusion.BTP.Metainfo
  doctest Effusion.BTP.Metainfo

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
    assert decode_result.info.pieces == expected_pieces
  end

  test "reject a non-bencode metainfo file" do
    assert {:error, :not_bencoded_form} = Metainfo.decode("abcdefg")
  end

  test "reject a malformed metainfo file" do
    {:ok, metainfo} = File.read("test/hello_world.torrent")
    # dictionaries need to be alphabetized by their key
    metainfo = :binary.replace(metainfo, "d6:lengthi6e4:pathl9:hello.txtee", "dl9:hello.txte6:lengthi6e4:pathe")

    assert {:error, :malformed_info_dict} = Metainfo.decode(metainfo)
  end

  test "inspect" do
    {:ok, metainfo} = File.read("test/linuxmint-18.3-cinnamon-64bit.iso.torrent")

    assert {:ok, decode_result} = Metainfo.decode(metainfo)

    assert inspect(decode_result) == "#Metainfo<[\"linuxmint-18.3-cinnamon-64bit.iso\" d2e53fb603652d991991b6ad2357a7a2845a5319]>"
  end

  test "multi-file torrent" do
    {:ok, metainfo} = File.read "test/lovecraft.torrent"

    {:ok, decode_result} = Metainfo.decode(metainfo)

    assert decode_result != nil
    assert Map.get(decode_result, :info_hash) == <<88, 143, 185, 194, 207, 159, 124, 235, 151, 57, 118, 193, 224, 234, 175, 56, 195, 68, 73, 153>>

    assert Enum.count(decode_result.info.files) == 52

    file = hd(decode_result.info.files)

    assert file.length == 93_355_085
    assert file.path == ["SevenH.P.LovecraftStories_librivox.m4b"]

  end
end
