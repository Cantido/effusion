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

  def tiny_meta do
    %{
      announce: "http://localhost:6969/announce",
      created_by: "Lovingly hand-crafted, by Rosa <3",
      creation_date: 1517785476,
      encoding: "UTF-8",
      info: %{
        :length => 5,
        :name => "tiny.txt",
        :piece_length => 3,
        :pieces => [
          <<242, 105, 25, 118, 134, 197, 108, 67, 163, 82, 84, 216, 119, 167, 25, 148, 192, 181, 112, 48>>,
          <<144, 99, 169, 240, 224, 50, 182, 35, 148, 3, 183, 25, 203, 187, 165, 106, 196, 228, 228, 95>>
      ],
        "private" => 0
      },
      info_hash: <<95, 189, 143, 1, 37, 56, 146, 40, 140, 78, 2, 250, 208, 144, 217, 10, 49, 7, 64, 28>>
    }
  end
end
