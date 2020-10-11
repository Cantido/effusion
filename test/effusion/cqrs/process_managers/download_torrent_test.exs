defmodule Effusion.CQRS.ProcessManagers.DownloadTorrentTest do
  use ExUnit.Case
  alias Effusion.CQRS.Events.PeerConnected
  alias Effusion.CQRS.Commands.SendBitfield
  alias Effusion.CQRS.ProcessManagers.DownloadTorrent
  alias Effusion.CQRS.Commands.SendBitfield
  doctest Effusion.CQRS.ProcessManagers.DownloadTorrent

  describe "handling PeerConnected" do
    test "Sends empty bitfield when we have no pieces" do
      peer_uuid = UUID.uuid4()
      command =
        DownloadTorrent.handle(
          %DownloadTorrent{pieces: IntSet.new()},
          %PeerConnected{peer_uuid: peer_uuid, info_hash: TestHelper.mint_info_hash(), initiated_by: "us"}
        )

      assert command.peer_uuid == peer_uuid
      assert command.bitfield == "00"
    end

    test "Sends bitfield when we have some pieces" do
      peer_uuid = UUID.uuid4()
      info_hash = TestHelper.mint_info_hash() |> Effusion.Hash.encode()
      pieces = IntSet.new([0, 1, 4, 8])
      expected_encoded_bitfield = IntSet.bitstring(pieces, byte_align: true) |> Base.encode16()

      command =
        DownloadTorrent.handle(
          %DownloadTorrent{pieces: pieces},
          %PeerConnected{peer_uuid: peer_uuid, info_hash: info_hash, initiated_by: "us"}
        )

      assert command.peer_uuid == peer_uuid
      assert command.bitfield == expected_encoded_bitfield
      end
  end
end
