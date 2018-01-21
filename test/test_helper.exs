Mox.defmock(Effusion.THP.Mock, for: Effusion.THP)

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
end
