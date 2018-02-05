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

  def mint_meta do
  %{
    announce: "https://torrents.linuxmint.com/announce.php",
    created_by: "Transmission/2.84 (14307)",
    creation_date: 1511774851,
    encoding: "UTF-8",
    info: %{
      :length => 1899528192,
      :name => "linuxmint-18.3-cinnamon-64bit.iso",
      :piece_length => 1048576,
      "pieces" =>
        <<167,  53,  69,  58,  13,
          103, 134, 251, 174, 104,
          105, 210,  94, 112, 197,
           52, 205, 246, 155, 130>>, # Just the SHA of the first piece
      "private" => 0
    },
    info_hash: <<210, 229, 63, 182, 3, 101, 45, 153, 25, 145, 182, 173, 35, 87,
      167, 162, 132, 90, 83, 25>>
  }
  end

  def hello_meta do
    %{
       announce: "http://localhost:6969/announce",
       created_by: "Transmission/2.84 (14307)",
       creation_date: 1517785476,
       encoding: "UTF-8",
       info: %{
         :length => 13,
         :name => "hello.txt",
         :piece_length => 32768,
         "pieces" => <<71, 160, 19, 230, 96, 212, 8, 97, 157, 137, 75, 32, 128, 107,
           29, 80, 134, 170, 176, 59>>,
         "private" => 0
       },
       info_hash: <<232, 31, 81, 18, 62, 247, 219, 231, 189, 30, 166, 92, 134, 3,
         243, 82, 13, 42, 188, 121>>
     }
  end
end
