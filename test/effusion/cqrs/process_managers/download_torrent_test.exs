defmodule Effusion.CQRS.ProcessManagers.DownloadTorrentTest do
  use ExUnit.Case
  alias Effusion.CQRS.ProcessManagers.DownloadTorrent
  alias Effusion.CQRS.Commands.{
    AttemptToConnect,
    SendBitfield
  }
  alias Effusion.CQRS.Events.{
    PeerAdded,
    PeerConnected
  }
  doctest Effusion.CQRS.ProcessManagers.DownloadTorrent

  describe "handling PeerAdded" do
    test "issues an AttemptToConnect command if we're not at our limt" do
      peer_uuid = UUID.uuid4()
      command =
        DownloadTorrent.handle(
          %DownloadTorrent{},
          %PeerAdded{peer_uuid: peer_uuid, expected_info_hash: "", expected_peer_id: "", host: {}, port: 0, from: "tracker"}
        )

      assert command == %AttemptToConnect{
        peer_uuid: peer_uuid
      }
    end
  end

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
