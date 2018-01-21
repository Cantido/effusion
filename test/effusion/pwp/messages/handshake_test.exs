alias Effusion.PWP.Messages.Handshake

defmodule Effusion.PWP.Messages.HandshakeTest do
  use ExUnit.Case
  doctest Effusion.PWP.Messages.Handshake

  @reserved_bytes <<0, 1, 2, 3, 4, 5, 6, 7>>

  @info_hash <<0, 1, 2, 3, 4,
               5, 6, 7, 8, 9,
               10, 11, 12, 13, 14,
               15, 16, 17, 18, 19>>

  @peer_id <<19, 18, 17, 16, 15,
             14, 13, 12, 11, 10,
             9, 8, 7, 6, 5,
             4, 3, 2, 1, 0>>

  test "decodes a handshake" do
    handshake = <<19>> <> "BitTorrent protocol" <> @reserved_bytes <> @info_hash <> @peer_id

    assert Handshake.decode(handshake) == {:ok, {@peer_id, @info_hash, @reserved_bytes}}
  end

  test "false to decode a handshake" do
    handshake = <<19>> <> "BitTorrent~protocol" <> @reserved_bytes <> @info_hash <> @peer_id

    assert Handshake.decode(handshake) == {:error, :malformed_handshake}
  end
end
