defmodule Effusion.DHT.Router do
  use Commanded.Commands.Router

  alias Effusion.CQRS.Aggregates.{
    Node,
    Peer,
    DHT
  }
  alias Effusion.CQRS.Commands

  dispatch [
    Commands.AddPeer
  ], to: Peer, identity: :peer_uuid

  dispatch [
    Commands.StartDHTNode
  ], to: DHT, identity: :node_id

  dispatch [
    Commands.GetPeers,
    Commands.AddDHTNode,
    Commands.HandlePeersMatching,
    Commands.HandleNodesNearest
  ], to: Node, identity: :node_id
end
