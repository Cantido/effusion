defmodule Effusion.CQRS.Router do
  use Commanded.Commands.Router
  alias Effusion.CQRS.Aggregates.{
    Torrent,
    Peer,
    Node
  }
  alias Effusion.CQRS.Commands

  dispatch [
    Commands.AddTorrent,
    Commands.StartDownload,
    Commands.StopDownload,
    Commands.StoreBlock,
    Commands.HandleCompletedDownload,
    Commands.EnableDHTForDownload
  ], to: Torrent, identity: :info_hash

  dispatch [
    Commands.AddPeer,
    Commands.AttemptToConnect,
    Commands.HandleFailedConnectionAttempt,
    Commands.AddOpenedPeerConnection,
    Commands.AddConnectedPeer,
    Commands.RemoveConnectedPeer,
    Commands.HandleBitfield,
    Commands.HandleCancel,
    Commands.HandleChoke,
    Commands.HandleHandshake,
    Commands.TimeoutHandshake,
    Commands.HandleHave,
    Commands.HandleInterested,
    Commands.HandlePiece,
    Commands.HandleRequest,
    Commands.HandleUnchoke,
    Commands.HandleUninterested,
    Commands.CancelRequest,
    Commands.RequestBlock,
    Commands.SendInterested,
    Commands.SendBitfield,
    Commands.SendHandshake,
    Commands.SendHave
  ], to: Peer, identity: :peer_uuid

  dispatch [
    Commands.GetPeers,
    Commands.AddDHTNode,
    Commands.StartDHTNode
  ], to: Node, identity: :node_id
end
