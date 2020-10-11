defmodule Effusion.CQRS.Aggregates.PeerTest do
  use Effusion.EventStoreCase
  alias Effusion.CQRS.Commands.{
    TimeoutHandshake,
    SendHandshake,
    HandleHandshake,
    RequestBlock
  }
  alias Effusion.CQRS.Events.{
    FailedHandshake,
    ConnectionAttemptFailed,
    SendingHandshake,
    SuccessfulHandshake,
    PeerSentHandshake,
    BlockRequested
  }
  alias Effusion.CQRS.Aggregates.Peer
  alias Commanded.Aggregate.Multi
  doctest Effusion.CQRS.Aggregates.Peer

  describe "Handling TimeoutHandshake" do
    test "emits FailedHandshake and ConnectionAttemptFailed when connection state is disconnected" do
      peer_uuid = UUID.uuid4()
      info_hash = ""
      events =
        Peer.execute(
          %Peer{
            peer_uuid: peer_uuid,
            info_hash: info_hash,
            connection_state: "disconnected"
          },
          %TimeoutHandshake{peer_uuid: peer_uuid}
        )

      assert events == [
        %FailedHandshake{
          peer_uuid: peer_uuid,
          failure_reason: "handshake timed out"
        },
        %ConnectionAttemptFailed{
          peer_uuid: peer_uuid,
          info_hash: info_hash,
          reason: "handshake timed out"
        }
      ]
    end
  end

  describe "Handling SendHandshake" do
    test "when intitated_by is 'us', just emit SendingHandshake" do
      peer_uuid = UUID.uuid4()
      info_hash = "12345678901234567890"
      our_peer_id = "Effusion Experiment!"
      our_extensions = [:fast, :dht]
      events =
        Peer.execute(
          %Peer{
            peer_uuid: peer_uuid,
            expected_info_hash: info_hash
          },
          %SendHandshake{
            peer_uuid: peer_uuid,
            our_peer_id: our_peer_id,
            our_extensions: our_extensions,
            initiated_by: "us"
          }
        )

      assert events ==
        %SendingHandshake{
          peer_uuid: peer_uuid,
          info_hash: info_hash,
          our_peer_id: our_peer_id,
          our_extensions: our_extensions,
          initiated_by: "us"
        }
    end

    test "when intitated_by is 'them', SendingHandshake and SuccessfulHandshake" do
      peer_uuid = UUID.uuid4()
      info_hash = "12345678901234567890"
      our_peer_id = "Effusion Experiment!"
      our_extensions = [:fast, :dht]
      events =
        Peer.execute(
          %Peer{
            peer_uuid: peer_uuid,
            expected_info_hash: info_hash
          },
          %SendHandshake{
            peer_uuid: peer_uuid,
            our_peer_id: our_peer_id,
            our_extensions: our_extensions,
            initiated_by: "them"
          }
        )

      assert events == [
        %SendingHandshake{
          peer_uuid: peer_uuid,
          info_hash: info_hash,
          our_peer_id: our_peer_id,
          our_extensions: our_extensions,
          initiated_by: "them"
        },
        %SuccessfulHandshake{
          peer_uuid: peer_uuid,
          initiated_by: "them"
        }
      ]
    end
  end

  describe "Handling HandleHandshake" do
    test "emits PeerSentHandshake when initiated by them" do
      peer_uuid = UUID.uuid4()
      info_hash = "12345678901234567890"
      peer_id = "Other peer~~~~~~~~~~"
      {_agg, events} =
        Peer.execute(
          %Peer{
            peer_uuid: peer_uuid,
            expected_info_hash: info_hash,
            expected_peer_id: peer_id
          },
          %HandleHandshake{
            peer_uuid: peer_uuid,
            info_hash: info_hash,
            peer_id: peer_id,
            extensions: [:dht, :fast],
            initiated_by: "them"
          }
        )
        |> Multi.run()

      assert events == [
        %PeerSentHandshake{
          peer_uuid: peer_uuid,
          info_hash: info_hash,
          peer_id: peer_id,
          initiated_by: "them"
        }
      ]
    end

    test "emits FailedHandshake when the info hash does not match what we expected" do
      peer_uuid = UUID.uuid4()
      info_hash = "12345678901234567890"
      peer_id = "Other peer~~~~~~~~~~"
      {_agg, events} =
        Peer.execute(
          %Peer{
            peer_uuid: peer_uuid,
            expected_info_hash: "09876543210987654321",
            expected_peer_id: peer_id
          },
          %HandleHandshake{
            peer_uuid: peer_uuid,
            info_hash: info_hash,
            peer_id: peer_id,
            extensions: [:dht, :fast],
            initiated_by: "them"
          }
        )
        |> Multi.run()

      assert events == [
        %PeerSentHandshake{
          peer_uuid: peer_uuid,
          info_hash: info_hash,
          peer_id: peer_id,
          initiated_by: "them"
        },
        %FailedHandshake{
          peer_uuid: peer_uuid,
          failure_reason: :info_hash
        }
      ]
    end

    test "emits FailedHandshake when the peer_id does not match what we expected" do
      peer_uuid = UUID.uuid4()
      info_hash = "12345678901234567890"
      peer_id = "Other peer~~~~~~~~~~"
      {_agg, events} =
        Peer.execute(
          %Peer{
            peer_uuid: peer_uuid,
            expected_info_hash: info_hash,
            expected_peer_id: "Another peer!!!!!!!!"
          },
          %HandleHandshake{
            peer_uuid: peer_uuid,
            info_hash: info_hash,
            peer_id: peer_id,
            extensions: [:dht, :fast],
            initiated_by: "them"
          }
        )
        |> Multi.run()

      assert events == [
        %PeerSentHandshake{
          peer_uuid: peer_uuid,
          info_hash: info_hash,
          peer_id: peer_id,
          initiated_by: "them"
        },
        %FailedHandshake{
          peer_uuid: peer_uuid,
          failure_reason: :peer_id
        }
      ]
    end

    test "emits SuccessfulHandshake when initiated_by us and all fields match" do
      peer_uuid = UUID.uuid4()
      info_hash = "12345678901234567890"
      peer_id = "Other peer~~~~~~~~~~"
      {_agg, events} =
        Peer.execute(
          %Peer{
            peer_uuid: peer_uuid,
            expected_info_hash: info_hash,
            expected_peer_id: peer_id
          },
          %HandleHandshake{
            peer_uuid: peer_uuid,
            info_hash: info_hash,
            peer_id: peer_id,
            extensions: [:dht, :fast],
            initiated_by: "us"
          }
        )
        |> Multi.run()

      assert events == [
        %PeerSentHandshake{
          peer_uuid: peer_uuid,
          info_hash: info_hash,
          peer_id: peer_id,
          initiated_by: "us"
        },
        %SuccessfulHandshake{
          peer_uuid: peer_uuid,
          initiated_by: "us"
        }
      ]
    end
  end

  describe "handle RequestBlock" do
    test "return an error if the peer is choking us" do
      peer_uuid = UUID.uuid4()
      info_hash = "12345678901234567890"
      ret =
        Peer.execute(
          %Peer{
            peer_uuid: peer_uuid,
            info_hash: info_hash,
            bitfield: IntSet.new(0),
            peer_choking: true
          },
          %RequestBlock{
            peer_uuid: peer_uuid,
            index: 0,
            offset: 0,
            size: 16384
          }
        )

      assert ret == {:error, "Peer is choking us, we cannot send requests"}
    end

    test "return an error if the peer does not have the block being requested" do
      peer_uuid = UUID.uuid4()
      info_hash = "12345678901234567890"
      ret =
        Peer.execute(
          %Peer{
            peer_uuid: peer_uuid,
            info_hash: info_hash,
            bitfield: IntSet.new(1),
            peer_choking: false
          },
          %RequestBlock{
            peer_uuid: peer_uuid,
            index: 0,
            offset: 0,
            size: 16384
          }
        )

      assert ret == {:error, "Cannot request a piece the peer does not have"}
    end

    test "return an error if we already requested this piece from this peer" do
      peer_uuid = UUID.uuid4()
      info_hash = "12345678901234567890"
      ret =
        Peer.execute(
          %Peer{
            peer_uuid: peer_uuid,
            info_hash: info_hash,
            bitfield: IntSet.new(0),
            peer_choking: false,
            our_requests: MapSet.new([{0, 0, 16384}])
          },
          %RequestBlock{
            peer_uuid: peer_uuid,
            index: 0,
            offset: 0,
            size: 16384
          }
        )

      assert ret == {:error, "Already requested this piece from this peer"}
    end

    test "return BlockRequested if everything is right" do
      peer_uuid = UUID.uuid4()
      info_hash = "12345678901234567890"
      ret =
        Peer.execute(
          %Peer{
            peer_uuid: peer_uuid,
            info_hash: info_hash,
            bitfield: IntSet.new(0),
            peer_choking: false,
            our_requests: MapSet.new()
          },
          %RequestBlock{
            peer_uuid: peer_uuid,
            index: 0,
            offset: 0,
            size: 16384
          }
        )

      assert ret ==
        %BlockRequested{
          peer_uuid: peer_uuid,
          info_hash: info_hash,
          index: 0,
          offset: 0,
          size: 16384
        }
    end
  end
end
