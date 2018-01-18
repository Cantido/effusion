defmodule Effusion.MetainfoTest do
  use ExUnit.Case
  alias Effusion.Metainfo
  doctest Effusion.Metainfo

  def mint_info_hash do
    # This value was obtained from a Transmission torrent client, so this
    # value is the known-correct info_hash for the Mint 18.3 64-bit torrent.
    <<210, 229,  63, 182,   3,
      101,  45, 153,  25, 145,
      182, 173,  35,  87, 167,
      162, 132,  90,  83,  25>>
  end


  test "adds info_hash to decoded file" do
    {:ok, metainfo} = File.read "test/linuxmint-18.3-cinnamon-64bit.iso.torrent"

    {:ok, decode_result} = Metainfo.decode(metainfo)

    assert decode_result != nil
    assert Map.get(decode_result, :info_hash) == mint_info_hash()
  end
end
