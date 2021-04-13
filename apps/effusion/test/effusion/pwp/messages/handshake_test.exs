defmodule Effusion.PWP.Messages.HandshakeTest do
  use ExUnit.Case, async: true
  alias Effusion.PWP.Messages.Handshake
  doctest Effusion.PWP.Messages.Handshake

  @reserved_bytes <<0, 0, 0, 0, 0, 0, 0, 0>>

  @info_hash <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19>>

  @peer_id <<19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0>>

  test "decodes a handshake" do
    handshake = <<19>> <> "BitTorrent protocol" <> @reserved_bytes <> @info_hash <> @peer_id

    assert Handshake.decode(handshake) ==
             {:ok, {:handshake, @peer_id, @info_hash, []}}
  end

  test "encodes a handshake" do
    encoded = Handshake.encode(@peer_id, @info_hash, [])

    expected = <<19>> <> "BitTorrent protocol" <> @reserved_bytes <> @info_hash <> @peer_id

    assert encoded == expected
  end

  test "false to decode a handshake" do
    handshake = <<19>> <> "BitTorrent~protocol" <> @reserved_bytes <> @info_hash <> @peer_id

    assert Handshake.decode(handshake) == {:error, :malformed_handshake}
  end

  test "decodes the fast extension bit in a handshake" do
    reserved_bytes = <<0, 0, 0, 0, 0, 0, 0, 0b00000100>>
    handshake = <<19>> <> "BitTorrent protocol" <> reserved_bytes <> @info_hash <> @peer_id

    {:ok, {:handshake, _peer_id, _info_hash, extensions}} = Handshake.decode(handshake)

    assert extensions == [:fast]
  end

  test "encodes a handshake with the fast extension" do
    encoded = Handshake.encode(@peer_id, @info_hash, [:fast])

    expected =
      <<19>> <>
        "BitTorrent protocol" <> <<0, 0, 0, 0, 0, 0, 0, 0b00000100>> <> @info_hash <> @peer_id

    assert encoded == expected
  end
end
