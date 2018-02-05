ExUnit.start(capture_log: true)

defmodule TestHelper do
  def mint_info_hash do
    # This value was obtained from a Transmission torrent client, so this
    # value is the known-correct info_hash for the Mint 18.3 64-bit torrent.
    <<210, 229,  63, 182,   3,
      101,  45, 153,  25, 145,
      182, 173,  35,  87, 167,
      162, 132,  90,  83,  25>>
  end

  def hello_info_hash do
    <<232,  31,  81,  18,  62,
      247, 219, 231, 189,  30,
      166,  92, 134,   3, 243,
       82,  13,  42, 188, 121>>
  end

  def mint_meta do
    {:ok, metabin} = File.read "test/linuxmint-18.3-cinnamon-64bit.iso.torrent"
    {:ok, meta} = Effusion.Metainfo.decode(metabin)
    meta
  end

  def hello_meta do
    {:ok, metabin} = File.read "test/hello.txt.torrent"
    {:ok, meta} = Effusion.Metainfo.decode(metabin)
    meta
  end
end
