defmodule Effusion.DHT.Router do
  use Commanded.Commands.Router

  alias Effusion.CQRS.Aggregates.{
    Node,
    Peer
  }
  alias Effusion.CQRS.Commands

  dispatch [
    Commands.AddPeer
  ], to: Peer, identity: :peer_uuid

  dispatch [
    Commands.GetPeers,
    Commands.AddDHTNode,
    Commands.StartDHTNode,
    Commands.HandlePeersMatching,
    Commands.HandleNodesNearest
  ], to: Node, identity: :node_id
end
