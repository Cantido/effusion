defmodule Effusion.CQRS.Aggregates.PeerTest do
  use Effusion.EventStoreCase
  alias Effusion.CQRS.Commands.{
    TimeoutHandshake,
    SendHandshake
  }
  alias Effusion.CQRS.Events.{
    FailedHandshake,
    ConnectionAttemptFailed,
    SendingHandshake,
    SuccessfulHandshake
  }
  alias Effusion.CQRS.Aggregates.Peer
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
            expected_info_hash: info_hash,
            connection_state: "disconnected"
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
            expected_info_hash: info_hash,
            connection_state: "disconnected"
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
end
