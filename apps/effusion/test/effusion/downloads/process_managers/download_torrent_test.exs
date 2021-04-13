defmodule Effusion.Downloads.ProcessManagers.DownloadTorrentTest do
  use ExUnit.Case
  alias Effusion.Factory
  alias Effusion.Downloads.ProcessManagers.DownloadTorrent
  alias Effusion.Downloads.Commands.StoreBlock
  alias Effusion.PWP.Connection.Commands.AttemptToConnect
  alias Effusion.PWP.Connection.Events.PeerConnected

  alias Effusion.PWP.Messages.Incoming.Events.{
    PeerHasBitfield,
    PeerUnchokedUs,
    PeerSentBlock
  }
  alias Effusion.PWP.Messages.Outgoing.Commands.{
    CancelRequest,
    SendInterested,
    RequestBlock
  }

  alias Effusion.PWP.Swarm.Events.PeerAddressAdded

  doctest Effusion.Downloads.ProcessManagers.DownloadTorrent

  describe "handling PeerAddressAdded" do
    test "issues an AttemptToConnect command if we're not at our limit" do
      peer_uuid = UUID.uuid4()

      command =
        DownloadTorrent.handle(
          %DownloadTorrent{},
          %PeerAddressAdded{
            peer_uuid: peer_uuid,
            expected_info_hash: Factory.encoded_info_hash(),
            expected_peer_id: Factory.encoded_peer_id(),
            host: {},
            port: 0,
            from: "tracker"
          }
        )

      assert command == %AttemptToConnect{
               peer_uuid: peer_uuid
             }
    end

    test "when we are at our connection limit, don't emit an AttemptToConnect command" do
      peer_uuid = UUID.uuid4()

      command =
        DownloadTorrent.handle(
          %DownloadTorrent{
            connected_peers: MapSet.new([UUID.uuid4()]),
            max_connections: 1
          },
          %PeerAddressAdded{
            peer_uuid: peer_uuid,
            expected_info_hash: Factory.encoded_info_hash(),
            expected_peer_id: Factory.encoded_peer_id(),
            host: {},
            port: 0,
            from: "tracker"
          }
        )

      assert is_nil(command)
    end

    test "when we are at our half-open connection limit, don't emit an AttemptToConnect command" do
      peer_uuid = UUID.uuid4()

      command =
        DownloadTorrent.handle(
          %DownloadTorrent{
            connecting_to_peers: MapSet.new([UUID.uuid4()]),
            max_connections: 1
          },
          %PeerAddressAdded{
            peer_uuid: peer_uuid,
            expected_info_hash: Factory.encoded_info_hash(),
            expected_peer_id: Factory.encoded_peer_id(),
            host: {},
            port: 0,
            from: "tracker"
          }
        )

      assert is_nil(command)
    end

    test "when our combo of conns and and half-opens is above our connection limit, don't emit an AttemptToConnect command" do
      peer_uuid = UUID.uuid4()

      command =
        DownloadTorrent.handle(
          %DownloadTorrent{
            connecting_to_peers: MapSet.new([UUID.uuid4()]),
            connected_peers: MapSet.new([UUID.uuid4()]),
            max_connections: 2,
            max_half_open_connections: 2
          },
          %PeerAddressAdded{
            peer_uuid: peer_uuid,
            expected_info_hash: Factory.encoded_info_hash(),
            expected_peer_id: Factory.encoded_peer_id(),
            host: {},
            port: 0,
            from: "tracker"
          }
        )

      assert is_nil(command)
    end
  end

  describe "handling PeerConnected" do
    test "Sends empty bitfield when we have no pieces" do
      peer_uuid = UUID.uuid4()

      command =
        DownloadTorrent.handle(
          %DownloadTorrent{pieces: IntSet.new()},
          %PeerConnected{
            peer_uuid: peer_uuid,
            info_hash: Factory.encoded_info_hash(),
            initiated_by: "us"
          }
        )

      assert command.peer_uuid == peer_uuid
      assert Base.decode64!(command.bitfield) |> IntSet.new() |> Enum.empty?()
    end

    test "Sends bitfield when we have some pieces" do
      peer_uuid = UUID.uuid4()
      info_hash = Factory.encoded_info_hash()
      pieces = IntSet.new([0, 1, 4, 8])
      expected_encoded_bitfield = IntSet.bitstring(pieces, byte_align: true) |> Base.encode64()

      command =
        DownloadTorrent.handle(
          %DownloadTorrent{pieces: pieces},
          %PeerConnected{
            peer_uuid: peer_uuid,
            info_hash: info_hash,
            initiated_by: "us"
          }
        )

      assert command.peer_uuid == peer_uuid
      assert command.bitfield == expected_encoded_bitfield
    end
  end

  describe "handling PeerHasBitfield" do
    test "when the bitfield contains a piece we want, commands SendInterested" do
      peer_uuid = UUID.uuid4()

      command =
        DownloadTorrent.handle(
          %DownloadTorrent{},
          %PeerHasBitfield{
            peer_uuid: peer_uuid,
            info_hash: Factory.encoded_info_hash(),
            bitfield: IntSet.new([1]) |> IntSet.bitstring(byte_align: true) |> Base.encode64()
          }
        )

      assert command == %SendInterested{
               peer_uuid: peer_uuid
             }
    end

    test "when the bitfield contains only pieces we ahve, does not send a command" do
      peer_uuid = UUID.uuid4()

      command =
        DownloadTorrent.handle(
          %DownloadTorrent{pieces: IntSet.new([1])},
          %PeerHasBitfield{
            peer_uuid: peer_uuid,
            info_hash: Factory.encoded_info_hash(),
            bitfield: IntSet.new([1]) |> IntSet.bitstring(byte_align: true) |> Base.encode64()
          }
        )

      assert is_nil(command)
    end
  end

  describe "handling PeerUnchokedUs" do
    test "if the peer has blocks we want, requests those blocks" do
      peer_uuid = UUID.uuid4()

      commands =
        DownloadTorrent.handle(
          %DownloadTorrent{
            target_piece_count: 1,
            max_requests_per_peer: 100,
            block_size: 16,
            info: %{piece_length: 16},
            peer_bitfields: %{peer_uuid => IntSet.new([0])}
          },
          %PeerUnchokedUs{peer_uuid: peer_uuid, info_hash: Factory.encoded_info_hash()}
        )

      assert commands == [
               %RequestBlock{
                 peer_uuid: peer_uuid,
                 index: 0,
                 offset: 0,
                 size: 16
               }
             ]
    end

    test "if the block size is bigger than piece length, request block of the piece length" do
      # This is a case we probably won't run into in real life,
      # but there's nothing in the protocol saying it can't
      peer_uuid = UUID.uuid4()

      commands =
        DownloadTorrent.handle(
          %DownloadTorrent{
            target_piece_count: 1,
            max_requests_per_peer: 100,
            block_size: 16,
            info: %{piece_length: 8},
            peer_bitfields: %{peer_uuid => IntSet.new([0])}
          },
          %PeerUnchokedUs{peer_uuid: peer_uuid, info_hash: Factory.encoded_info_hash()}
        )

      assert commands == [
               %RequestBlock{
                 peer_uuid: peer_uuid,
                 index: 0,
                 offset: 0,
                 size: 8
               }
             ]
    end

    test "if the peer does not have blocks we want, don't request" do
      peer_uuid = UUID.uuid4()

      commands =
        DownloadTorrent.handle(
          %DownloadTorrent{
            target_piece_count: 1,
            max_requests_per_peer: 100,
            block_size: 16,
            info: %{piece_length: 16},
            peer_bitfields: %{}
          },
          %PeerUnchokedUs{peer_uuid: peer_uuid, info_hash: Factory.encoded_info_hash()}
        )

      assert commands == []
    end

    test "if we've already requested all the pieces the peer has, don't request" do
      peer_uuid = UUID.uuid4()

      commands =
        DownloadTorrent.handle(
          %DownloadTorrent{
            target_piece_count: 1,
            max_requests_per_peer: 100,
            block_size: 16,
            info: %{piece_length: 16},
            requests: %{{0, 0, 16} => MapSet.new([peer_uuid])},
            peer_bitfields: %{peer_uuid => IntSet.new([0])}
          },
          %PeerUnchokedUs{peer_uuid: peer_uuid, info_hash: Factory.encoded_info_hash()}
        )

      assert commands == []
    end

    test "if we're at our request-per-peer limit, don't request" do
      peer_uuid = UUID.uuid4()

      commands =
        DownloadTorrent.handle(
          %DownloadTorrent{
            target_piece_count: 1,
            max_requests_per_peer: 1,
            block_size: 16,
            info: %{piece_length: 16},
            requests: %{{0, 0, 16} => MapSet.new([peer_uuid])},
            peer_bitfields: %{peer_uuid => IntSet.new([0])}
          },
          %PeerUnchokedUs{peer_uuid: peer_uuid, info_hash: Factory.encoded_info_hash()}
        )

      assert commands == []
    end

    test "make sure we can understand offsets" do
      peer_uuid = UUID.uuid4()

      commands =
        DownloadTorrent.handle(
          %DownloadTorrent{
            target_piece_count: 1,
            max_requests_per_peer: 100,
            block_size: 16,
            info: %{piece_length: 32},
            requests: %{{0, 0, 16} => MapSet.new([peer_uuid])},
            peer_bitfields: %{peer_uuid => IntSet.new([0])}
          },
          %PeerUnchokedUs{peer_uuid: peer_uuid, info_hash: Factory.encoded_info_hash()}
        )

      assert commands == [
               %RequestBlock{
                 peer_uuid: peer_uuid,
                 index: 0,
                 offset: 16,
                 size: 16
               }
             ]
    end
  end

  describe "handling PeerSentBlock" do
    test "always sends sends StoreBlock" do
      peer_uuid = UUID.uuid4()
      data = :crypto.strong_rand_bytes(16) |> Base.encode64()
      info_hash = Factory.encoded_info_hash()

      commands =
        DownloadTorrent.handle(
          %DownloadTorrent{
            target_piece_count: 1,
            max_requests_per_peer: 100,
            block_size: 16,
            info: %{piece_length: 16}
          },
          %PeerSentBlock{
            peer_uuid: peer_uuid,
            info_hash: info_hash,
            index: 0,
            offset: 0,
            data: data
          }
        )

      assert commands == [
               %StoreBlock{
                 from: peer_uuid,
                 info_hash: info_hash,
                 index: 0,
                 offset: 0,
                 data: data
               }
             ]
    end

    test "when we had a request to that same peer for that block, don't send cancel" do
      peer_uuid = UUID.uuid4()
      data = :crypto.strong_rand_bytes(16) |> Base.encode64()
      info_hash = Factory.encoded_info_hash()

      commands =
        DownloadTorrent.handle(
          %DownloadTorrent{
            target_piece_count: 1,
            max_requests_per_peer: 100,
            block_size: 16,
            info: %{piece_length: 16},
            requests: %{{0, 0, 16} => MapSet.new([peer_uuid])}
          },
          %PeerSentBlock{
            peer_uuid: peer_uuid,
            info_hash: info_hash,
            index: 0,
            offset: 0,
            data: data
          }
        )

      assert commands == [
               %StoreBlock{
                 from: peer_uuid,
                 info_hash: info_hash,
                 index: 0,
                 offset: 0,
                 data: data
               }
             ]
    end

    test "when we had a request to another peer for that block, send cancel" do
      peer_uuid = UUID.uuid4()
      other_peer_uuid = UUID.uuid4()
      data = :crypto.strong_rand_bytes(16) |> Base.encode64()
      info_hash = Factory.encoded_info_hash()

      commands =
        DownloadTorrent.handle(
          %DownloadTorrent{
            target_piece_count: 1,
            max_requests_per_peer: 100,
            block_size: 16,
            info: %{piece_length: 16},
            requests: %{{0, 0, 16} => MapSet.new([other_peer_uuid])}
          },
          %PeerSentBlock{
            peer_uuid: peer_uuid,
            info_hash: info_hash,
            index: 0,
            offset: 0,
            data: data
          }
        )

      assert commands == [
               %StoreBlock{
                 from: peer_uuid,
                 info_hash: info_hash,
                 index: 0,
                 offset: 0,
                 data: data
               },
               %CancelRequest{
                 peer_uuid: other_peer_uuid,
                 index: 0,
                 offset: 0,
                 size: 16
               }
             ]
    end

    test "when that peer has other blocks we want, request them" do
      peer_uuid = UUID.uuid4()
      data = :crypto.strong_rand_bytes(16) |> Base.encode64()
      info_hash = Factory.encoded_info_hash()

      commands =
        DownloadTorrent.handle(
          %DownloadTorrent{
            target_piece_count: 1,
            max_requests_per_peer: 100,
            block_size: 16,
            info: %{piece_length: 32},
            peer_bitfields: %{peer_uuid => IntSet.new(0)}
          },
          %PeerSentBlock{
            peer_uuid: peer_uuid,
            info_hash: info_hash,
            index: 0,
            offset: 0,
            data: data
          }
        )

      assert commands == [
               %StoreBlock{
                 from: peer_uuid,
                 info_hash: info_hash,
                 index: 0,
                 offset: 0,
                 data: data
               },
               %RequestBlock{
                 peer_uuid: peer_uuid,
                 index: 0,
                 offset: 16,
                 size: 16
               }
             ]
    end
  end

  describe "applies PeerSentBlock" do
    test "adds block to blocks map" do
      peer_uuid = UUID.uuid4()
      data = :crypto.strong_rand_bytes(16) |> Base.encode64()

      download =
        DownloadTorrent.apply(
          %DownloadTorrent{
            blocks: %{}
          },
          %PeerSentBlock{
            peer_uuid: peer_uuid,
            info_hash: Factory.encoded_info_hash(),
            index: 0,
            offset: 0,
            data: data
          }
        )

      assert download.blocks == %{0 => IntSet.new(0)}
    end

    test "deletes block from requests map" do
      peer_uuid = UUID.uuid4()
      data = :crypto.strong_rand_bytes(16) |> Base.encode64()

      download =
        DownloadTorrent.apply(
          %DownloadTorrent{
            requests: %{{0, 0, 16} => MapSet.new([peer_uuid])}
          },
          %PeerSentBlock{
            peer_uuid: peer_uuid,
            info_hash: Factory.encoded_info_hash(),
            index: 0,
            offset: 0,
            data: data
          }
        )

      assert download.requests == %{{0, 0, 16} => MapSet.new()}
    end
  end
end
