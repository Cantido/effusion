defmodule Effusion.CQRS.Contexts.DHT do
  alias Effusion.CQRS.Application, as: CQRS
  alias Effusion.CQRS.Commands.{
    AddPeer,
    HandlePeersMatching
  }

  def start_dht(node_id) do
    CQRS.dispatch(%Effusion.CQRS.Commands.StartDHTNode{
      node_id: Effusion.Hash.encode(node_id)
    })
  end

  def enable_dht_for_download(info_hash, primary_node_id) do
    CQRS.dispatch(
      %Effusion.CQRS.Commands.EnableDHTForDownload{
        node_id: Effusion.Hash.encode(primary_node_id),
        info_hash: Effusion.Hash.encode(info_hash)
      }
    )
  end

  def add_node(primary_node_id, remote_node_id, host, port) do
    CQRS.dispatch(
      %Effusion.CQRS.Commands.AddDHTNode{
        primary_node_id: Effusion.Hash.encode(primary_node_id),
        node_id: Effusion.Hash.encode(remote_node_id),
        host: to_string(:inet.ntoa(host)),
        port: port
      }
    )
  end

  def handle_peers_matching(remote_node_id, transaction_id, token, peers) do
    stringified_peers = Enum.map(peers, fn {host, port} ->
      {to_string(:inet.ntoa(host)), port}
    end)
    CQRS.dispatch(
      %Effusion.CQRS.Commands.HandlePeersMatching{
        node_id: Effusion.Hash.encode(remote_node_id),
        transaction_id: transaction_id,
        token: token,
        peers: stringified_peers
      }
    )
  end

  def mark_node_as_contacted(node_id, contacted_at) do

  end

  def issue_token(node_id, info_hash, token, expiry) do
    CQRS.dispatch(
      %Effusion.CQRS.Commands.IssueToken{
        node_id: Effusion.Hash.encode(node_id),
        info_hash: Effusion.Hash.encode(info_hash),
        token: token,
        expires_at: expiry
      }
    )
  end

  def accept_token(token, host, port) do

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
