defmodule Effusion.CQRS.ProcessManagers.DHTProtocolTest do
  use Effusion.EventStoreCase
  alias Effusion.Factory
  alias Effusion.DHT
  alias Effusion.CQRS.Application, as: CQRS
  alias Effusion.CQRS.Commands.{
    AddDHTNode,
    AddTorrent,
    StartDHTNode,
    StartDownload,
    EnableDHTForDownload,
    HandlePeersMatching
  }
  alias Effusion.CQRS.Events.{
    DHTNodeStarted,
    DHTNodeAdded,
    GettingPeers,
    DHTEnabledForDownload,
    PeerAdded
  }
  import Commanded.Assertions.EventAssertions

  setup do
    primary_node_id = Factory.encoded_node_id()

    meta = TestHelper.mint_meta()
    info_hash = Effusion.Hash.encode(TestHelper.mint_info_hash())
    other_node_id = Factory.encoded_node_id()

    :ok = CQRS.dispatch(%AddTorrent{
      announce: meta.announce,
      announce_list: meta.announce_list,
      comment: meta.comment,
      created_by: meta.created_by,
      creation_date: meta.creation_date,
      info: meta.info,
      info_hash: info_hash
    })

    :ok = CQRS.dispatch(%StartDHTNode{
      node_id: primary_node_id
    })

    %AddDHTNode{
      primary_node_id: primary_node_id,
      node_id: other_node_id,
      host: {127, 0, 0, 1},
      port: 6881
    } |> CQRS.dispatch()

    :ok = CQRS.dispatch(%EnableDHTForDownload{
      node_id: primary_node_id,
      info_hash: info_hash
    })

    %StartDownload{
      info_hash: info_hash,
      block_size: 16,
      max_requests_per_peer: 1000,
      max_half_open_connections: 1000,
      max_connections: 200
    } |> CQRS.dispatch()

    %{
      primary_node_id: primary_node_id,
      info_hash: info_hash,
      other_node_id: other_node_id
    }
  end

  test "when a download is started, ask the only node in the DHT routing table for peers",
  %{primary_node_id: primary_node_id, info_hash: info_hash, other_node_id: other_node_id} do

    assert_receive_event(CQRS, DHTNodeAdded, fn event ->
      assert event.primary_node_id == primary_node_id
      assert event.node_id == other_node_id
      assert event.host == {127, 0, 0, 1}
      assert event.port == 6881
    end)

    assert_receive_event(CQRS, GettingPeers, fn event ->
      assert event.primary_node_id == primary_node_id
      assert event.node_id == other_node_id
      assert event.host == {127, 0, 0, 1}
      assert event.port == 6881
      assert event.info_hash == info_hash
      assert not is_nil(event.transaction_id)
    end)
  end



  test "when DHT gets a peer back, it will emit an AddPeer for that peer",
  %{primary_node_id: primary_node_id, info_hash: info_hash, other_node_id: other_node_id} do
    token = DHT.token()

    assert_receive_event(CQRS, GettingPeers, fn event ->
      :ok = CQRS.dispatch(%HandlePeersMatching{
        transaction_id: event.transaction_id,
        node_id: other_node_id,
        token: token,
        peers: [
          {{127, 0, 0, 1}, 9001}
        ]
      })
    end)

    assert_receive_event(CQRS, PeerAdded, fn event ->
      assert not is_nil(event.peer_uuid)
      assert event.expected_info_hash == info_hash
      assert event.host == {127, 0, 0, 1}
      assert event.port == 9001
      assert event.from == "dht"
    end)
  end
end
