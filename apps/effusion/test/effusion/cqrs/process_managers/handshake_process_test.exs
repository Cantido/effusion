defmodule Effusion.CQRS.Aggregates.HandshakeProcessTest do
  use Effusion.EventStoreCase
  alias Effusion.Commanded, as: CQRS
  alias Effusion.CQRS.Commands.{
    AddTorrent,
    StartDownload,
    AddPeer,
    AttemptToConnect,
    AddOpenedPeerConnection,
    SendHandshake,
    HandleHandshake
  }
  alias Effusion.CQRS.Events.{
    PeerAddressAdded,
    PeerConnectionOpened,
    PeerSentHandshake,
    SendingHandshake,
    SuccessfulHandshake,
    FailedHandshake
  }
  import Commanded.Assertions.EventAssertions

  describe "handshake process" do
    test "when the other peer initiated" do
      peer_uuid = UUID.uuid4()
      meta = TestHelper.mint_meta()
      info_hash = Effusion.Hash.encode(TestHelper.mint_info_hash())
      our_peer_id = Effusion.Hash.encode("Effusion Experiment!")
      our_extensions = [:dht]
      remote_peer_id = Effusion.Hash.encode("Other peer ID~~~~~~~")
      remote_extensions = [:dht, :fast]
      host = "127.0.0.1"
      port = 6801

      :ok = CQRS.dispatch(%AddTorrent{
        announce: meta.announce,
        announce_list: meta.announce_list,
        comment: meta.comment,
        created_by: meta.created_by,
        creation_date: meta.creation_date,
        info: meta.info,
        info_hash: info_hash
      })
      :ok = CQRS.dispatch(%StartDownload{
        info_hash: info_hash,
        block_size: Application.fetch_env!(:effusion, :block_size),
        max_requests_per_peer: 1,
        max_half_open_connections: 2,
        max_connections: 1
      })
      :ok = CQRS.dispatch(%AddPeerAddress{
        peer_uuid: peer_uuid,
        expected_info_hash: info_hash,
        expected_peer_id: remote_peer_id,
        host: host,
        port: port,
        from: :connection
      })
      :ok = CQRS.dispatch(%HandleHandshake{
        peer_uuid: peer_uuid,
        info_hash: info_hash,
        peer_id: remote_peer_id,
        extensions: remote_extensions,
        initiated_by: "them"
      })
      :ok = CQRS.dispatch(%SendHandshake{
        peer_uuid: peer_uuid,
        our_peer_id: our_peer_id,
        our_extensions: our_extensions,
        initiated_by: "them"
      })

      assert_receive_event(CQRS, PeerAddressAdded, fn event ->
        assert event.peer_uuid == peer_uuid
        assert event.expected_info_hash == info_hash
        assert event.expected_peer_id == remote_peer_id
        assert event.host == host
        assert event.port == port
        assert event.from == :connection
      end)
      assert_receive_event(CQRS, PeerSentHandshake, fn event ->
        assert event.peer_uuid == peer_uuid
        assert event.info_hash == info_hash
        assert event.peer_id == remote_peer_id
        assert event.initiated_by == "them"
      end)
      assert_receive_event(CQRS, SendingHandshake, fn event ->
        assert event.peer_uuid == peer_uuid
        assert event.our_peer_id == our_peer_id
        assert event.our_extensions == our_extensions
        assert event.initiated_by == "them"
      end)
      assert_receive_event(CQRS, SuccessfulHandshake, fn event ->
        assert event.peer_uuid == peer_uuid
        assert event.initiated_by == "them"
      end)
    end

    test "when we initiated" do
      peer_uuid = UUID.uuid4()
      meta = TestHelper.mint_meta()
      info_hash = Effusion.Hash.encode(TestHelper.mint_info_hash())
      our_peer_id = Effusion.Hash.encode("Effusion Experiment!")
      our_extensions = [:dht]
      remote_peer_id = Effusion.Hash.encode("Other peer ID~~~~~~~")
      remote_extensions = [:dht, :fast]
      host = "127.0.0.1"
      port = 6801

      :ok = CQRS.dispatch(%AddTorrent{
        announce: meta.announce,
        announce_list: meta.announce_list,
        comment: meta.comment,
        created_by: meta.created_by,
        creation_date: meta.creation_date,
        info: meta.info,
        info_hash: info_hash
      })
      :ok = CQRS.dispatch(%StartDownload{
        info_hash: info_hash,
        block_size: Application.fetch_env!(:effusion, :block_size),
        max_requests_per_peer: 1,
        max_half_open_connections: 2,
        max_connections: 1
      })
      :ok = CQRS.dispatch(%AddPeerAddress{
        peer_uuid: peer_uuid,
        expected_info_hash: info_hash,
        expected_peer_id: remote_peer_id,
        host: host,
        port: port,
        from: :tracker
      })
      :ok = CQRS.dispatch(%AttemptToConnect{
        peer_uuid: peer_uuid
      })
      :ok = CQRS.dispatch(%AddOpenedPeerConnection{
          peer_uuid: peer_uuid,
          host: host,
          port: port
      })
      :ok = CQRS.dispatch(%SendHandshake{
          peer_uuid: peer_uuid,
          our_peer_id: our_peer_id,
          our_extensions: our_extensions,
          initiated_by: "us"
      })
      :ok = CQRS.dispatch(%HandleHandshake{
        peer_uuid: peer_uuid,
        info_hash: info_hash,
        peer_id: remote_peer_id,
        extensions: remote_extensions,
        initiated_by: "us"
      })

      assert_receive_event(CQRS, PeerAddressAdded, fn event ->
        assert event.peer_uuid == peer_uuid
        assert event.expected_info_hash == info_hash
        assert event.expected_peer_id == remote_peer_id
        assert event.host == host
        assert event.port == port
        assert event.from == :tracker
      end)
      assert_receive_event(CQRS, PeerConnectionOpened, fn event ->
        assert event.peer_uuid == peer_uuid
      end)
      assert_receive_event(CQRS, SendingHandshake, fn event ->
        assert event.peer_uuid == peer_uuid
        assert event.our_peer_id == our_peer_id
        assert event.our_extensions == our_extensions
        assert event.initiated_by == "us"
      end)
      assert_receive_event(CQRS, PeerSentHandshake, fn event ->
        assert event.peer_uuid == peer_uuid
        assert event.info_hash == info_hash
        assert event.peer_id == remote_peer_id
        assert event.initiated_by == "us"
      end)
      assert_receive_event(CQRS, SuccessfulHandshake, fn event ->
        assert event.peer_uuid == peer_uuid
        assert event.initiated_by == "us"
      end)
    end

    test "when we initiated and the other peer sends the wrong info hash" do
      peer_uuid = UUID.uuid4()
      info_hash = "5fbd8f01253892288c4e02fad090d90a3107401c"
      wrong_info_hash = "5fbd8f01253892288c4e02fad090d90a3107401d"
      our_peer_id = Effusion.Hash.encode("Effusion Experiment!")
      our_extensions = [:dht]
      remote_peer_id = Effusion.Hash.encode("Other peer ID~~~~~~~")
      remote_extensions = [:dht, :fast]
      host = "127.0.0.1"
      port = 6801

      :ok = CQRS.dispatch(%AddPeerAddress{
        peer_uuid: peer_uuid,
        expected_info_hash: info_hash,
        expected_peer_id: remote_peer_id,
        host: host,
        port: port,
        from: :tracker
      })
      :ok = CQRS.dispatch(%AttemptToConnect{
        peer_uuid: peer_uuid
      })
      :ok = CQRS.dispatch(%SendHandshake{
        peer_uuid: peer_uuid,
        our_peer_id: our_peer_id,
        our_extensions: our_extensions,
        initiated_by: "us"
      })
      :ok = CQRS.dispatch(%HandleHandshake{
        peer_uuid: peer_uuid,
        info_hash: wrong_info_hash,
        peer_id: remote_peer_id,
        extensions: remote_extensions,
        initiated_by: "us"
      })

      assert_receive_event(CQRS, PeerAddressAdded, fn event ->
        assert event.peer_uuid == peer_uuid
        assert event.expected_info_hash == info_hash
        assert event.expected_peer_id == remote_peer_id
        assert event.host == host
        assert event.port == port
        assert event.from == :tracker
      end)
      assert_receive_event(CQRS, SendingHandshake, fn event ->
        assert event.peer_uuid == peer_uuid
        assert event.our_peer_id == our_peer_id
        assert event.our_extensions == our_extensions
        assert event.initiated_by == "us"
      end)
      assert_receive_event(CQRS, PeerSentHandshake, fn event ->
        assert event.peer_uuid == peer_uuid
        assert event.info_hash == wrong_info_hash
        assert event.peer_id == remote_peer_id
        assert event.initiated_by == "us"
      end)
      assert_receive_event(CQRS, FailedHandshake, fn event ->
        assert event.peer_uuid == peer_uuid
        assert event.failure_reason == :info_hash
      end)
    end

    test "when the other peer initiated and sends the wrong info hash" do
      peer_uuid = UUID.uuid4()
      meta = TestHelper.mint_meta()
      info_hash = Effusion.Hash.encode(TestHelper.mint_info_hash())
      wrong_info_hash = "5fbd8f01253892288c4e02fad090d90a3107401d"
      our_peer_id = Effusion.Hash.encode("Effusion Experiment!")
      our_extensions = [:dht]
      remote_peer_id = Effusion.Hash.encode("Other peer ID~~~~~~~")
      remote_extensions = [:dht, :fast]
      host = "127.0.0.1"
      port = 6801

      :ok = CQRS.dispatch(%AddTorrent{
        announce: meta.announce,
        announce_list: meta.announce_list,
        comment: meta.comment,
        created_by: meta.created_by,
        creation_date: meta.creation_date,
        info: meta.info,
        info_hash: info_hash
      })
      :ok = CQRS.dispatch(%StartDownload{
        info_hash: info_hash,
        block_size: Application.fetch_env!(:effusion, :block_size),
        max_requests_per_peer: 1,
        max_half_open_connections: 2,
        max_connections: 1
      })
      :ok = CQRS.dispatch(%AddPeerAddress{
        peer_uuid: peer_uuid,
        expected_info_hash: info_hash,
        expected_peer_id: remote_peer_id,
        host: host,
        port: port,
        from: :connection
      })
      :ok = CQRS.dispatch(%HandleHandshake{
        peer_uuid: peer_uuid,
        info_hash: wrong_info_hash,
        peer_id: remote_peer_id,
        extensions: remote_extensions,
        initiated_by: "them"
      })
      :ok = CQRS.dispatch(%SendHandshake{
        peer_uuid: peer_uuid,
        our_peer_id: our_peer_id,
        our_extensions: our_extensions,
        initiated_by: "them"
      })

      assert_receive_event(CQRS, PeerAddressAdded, fn event ->
        assert event.peer_uuid == peer_uuid
        assert event.expected_info_hash == info_hash
        assert event.expected_peer_id == remote_peer_id
        assert event.host == host
        assert event.port == port
        assert event.from == :connection
      end)
      assert_receive_event(CQRS, PeerSentHandshake, fn event ->
        assert event.peer_uuid == peer_uuid
        assert event.info_hash == wrong_info_hash
        assert event.peer_id == remote_peer_id
        assert event.initiated_by == "them"
      end)
      assert_receive_event(CQRS, FailedHandshake, fn event ->
        assert event.peer_uuid == peer_uuid
        assert event.failure_reason == :info_hash
      end)
    end
  end
end
