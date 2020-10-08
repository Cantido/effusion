defmodule Effusion.CQRS.Router do
  use Commanded.Commands.Router
  alias Effusion.CQRS.Aggregates.{
    Torrent,
    Peer
  }
  alias Effusion.CQRS.Commands

  dispatch [
    Commands.AddTorrent,
    Commands.StartDownload,
    Commands.StopDownload,
    Commands.StoreBlock,
    Commands.HandleCompletedDownload
  ], to: Torrent, identity: :info_hash

  dispatch [
    Commands.AddPeer,
    Commands.AttemptToConnect,
    Commands.AddConnectedPeer,
    Commands.RemoveConnectedPeer,
    Commands.HandleBitfield,
    Commands.HandleCancel,
    Commands.HandleChoke,
    Commands.HandleHandshake,
    Commands.HandleHave,
    Commands.HandleInterested,
    Commands.HandlePiece,
    Commands.HandleRequest,
    Commands.HandleUnchoke,
    Commands.HandleUninterested,
    Commands.RequestBlock,
    Commands.SendInterested,
    Commands.SendBitfield
  ], to: Peer, identity: :internal_peer_id
end
