defmodule Effusion.CQRS.Contexts.DHT do
  alias Effusion.CQRS.Application, as: CQRS
  alias Effusion.CQRS.Commands.{
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

  def handle_peers_matching(transaction_id, node_id, token, peers) do
    CQRS.dispatch(
      %HandlePeersMatching{
        transaction_id: transaction_id,
        node_id: Effusion.Hash.encode(node_id),
        token: token,
        peers: Enum.map(peers, fn {host, port} ->
          {to_string(:inet.ntoa(host)), port}
        end)
      }
    )
  end
end
