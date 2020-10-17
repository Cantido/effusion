defmodule Effusion.CQRS.Contexts.DHT do
  alias Effusion.DHT.CQRS
  alias Effusion.CQRS.Commands.{
    AddPeer,
    AddDHTNode,
    StartDHTNode,
    RefreshNode,
    EnableDHTForDownload,
    HandlePeersMatching,
    HandleNodesNearest,
    IssueToken,
    AcceptToken
  }

  def start_dht(node_id) do
    CQRS.dispatch(%StartDHTNode{
      node_id: Effusion.Hash.encode(node_id)
    })
  end

  def enable_dht_for_download(info_hash, primary_node_id) do
    CQRS.dispatch(
      %EnableDHTForDownload{
        node_id: Effusion.Hash.encode(primary_node_id),
        info_hash: Effusion.Hash.encode(info_hash)
      }
    )
  end

  def handle_peers_matching(remote_node_id, transaction_id, token, peers) do
    stringified_peers = Enum.map(peers, fn {host, port} ->
      {to_string(:inet.ntoa(host)), port}
    end)
    CQRS.dispatch(
      %HandlePeersMatching{
        node_id: Effusion.Hash.encode(remote_node_id),
        transaction_id: transaction_id,
        token: token,
        peers: stringified_peers
      }
    )
  end

  def handle_nodes_nearest(remote_node_id, transaction_id, token, nodes) do
    stringified_nodes = Enum.map(nodes, fn {node_id, {host, port}} ->
      {
        Effusion.Hash.encode(node_id),
        {to_string(:inet.ntoa(host)), port}
      }

    end)
    CQRS.dispatch(
      %HandleNodesNearest{
        transaction_id: transaction_id,
        node_id: Effusion.Hash.encode(remote_node_id),
        token: token,
        nodes: stringified_nodes
      }
    )
  end

  def mark_node_as_contacted(node_id, last_contacted) do
    CQRS.dispatch(
      %RefreshNode{
        node_id: Effusion.Hash.encode(node_id),
        last_contacted: last_contacted
      }
    )
  end

  def add_peer(info_hash, host, port) do
    %AddPeer{
      peer_uuid: UUID.uuid4(),
      expected_info_hash: Effusion.Hash.encode(info_hash),
      host: to_string(:inet.ntoa(host)),
      port: port,
      from: "dht"}
    |> CQRS.dispatch()
  end
end
