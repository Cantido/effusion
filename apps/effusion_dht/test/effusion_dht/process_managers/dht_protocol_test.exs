defmodule Effusion.CQRS.ProcessManagers.DHTProtocolTest do
  use Effusion.DHT.EventStoreCase
  alias Effusion.Factory
  alias Effusion.DHT
  alias Effusion.DHT.Nodes
  alias Effusion.Downloads, as: DownloadsContext
  alias Effusion.PWP, as: PeersContext
  alias Effusion.CQRS.Contexts.DHT, as: DHTContext
  alias Effusion.DHT.CQRS
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
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Effusion.Repo, { :shared, self() })
  end

  setup do
    primary_node_id = DHT.node_id()

    meta = TestHelper.mint_meta()
    info_hash = TestHelper.mint_info_hash()
    other_node_id = "other node id~~~~~~~"

    :ok = DownloadsContext.add(meta)
    :ok = DHTContext.start_dht(primary_node_id)
    :ok = Nodes.add(
      primary_node_id,
      other_node_id,
      {127, 0, 0, 1},
      6881
    )

    :ok = DHTContext.enable_dht_for_download(
      info_hash,
      primary_node_id
    )
    :ok = DownloadsContext.start(info_hash, 16, 1000, 1000, 200)

    %{
      primary_node_id: primary_node_id,
      info_hash: info_hash,
      other_node_id: other_node_id
    }
  end

  test "when a download is started, ask the only node in the DHT routing table for peers",
  %{primary_node_id: primary_node_id, info_hash: info_hash, other_node_id: other_node_id} do

    assert_receive_event(CQRS, DHTNodeAdded, fn event ->
      assert event.primary_node_id == Effusion.Hash.encode(primary_node_id)
      assert event.node_id == Effusion.Hash.encode(other_node_id)
      assert event.host == to_string(:inet.ntoa({127, 0, 0, 1}))
      assert event.port == 6881
    end)

    assert_receive_event(CQRS, GettingPeers, fn event ->
      assert event.primary_node_id == Effusion.Hash.encode(primary_node_id)
      assert event.node_id == Effusion.Hash.encode(other_node_id)
      assert event.host == to_string(:inet.ntoa({127, 0, 0, 1}))
      assert event.port == 6881
      assert event.info_hash == Effusion.Hash.encode(info_hash)
      assert not is_nil(event.transaction_id)
    end)
  end

  test "when DHT gets nearest nodes back, it will emit a DHTNodeAdded for them",
  %{primary_node_id: primary_node_id, info_hash: info_hash, other_node_id: other_node_id} do
    other_other_node_id = DHT.node_id()
    token = DHT.token()

    assert_receive_event(CQRS, GettingPeers, fn event ->
      :ok = DHTContext.handle_nodes_nearest(
        other_node_id,
        event.transaction_id,
        token,
        [{other_other_node_id, {{127, 0, 0, 1}, 7000}}]
      )
    end)

    assert_receive_event(CQRS, DHTNodeAdded,
    fn event -> event.node_id == Effusion.Hash.encode(other_other_node_id) end,
    fn event ->
      assert event.primary_node_id == Effusion.Hash.encode(primary_node_id)
      assert event.node_id == Effusion.Hash.encode(other_other_node_id)
      assert event.host == to_string(:inet.ntoa({127, 0, 0, 1}))
      assert event.port == 7000
    end)
  end
end
