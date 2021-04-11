defmodule Effusion.DHT.Router do
  use Commanded.Commands.Router

  alias Effusion.CQRS.Aggregates.{
    Node,
    DHT
  }
  alias Effusion.CQRS.Commands

  dispatch [
    Commands.StartDHTNode,
    Commands.EnableDHTForDownload
  ], to: DHT, identity: :node_id

  dispatch [
    Commands.GetPeers,
    Commands.AddDHTNode,
    Commands.HandlePeersMatching,
    Commands.HandleNodesNearest
  ], to: Node, identity: :node_id
end
