defmodule Effusion.CQRS.Router do
  use Commanded.Commands.Router
  alias Effusion.CQRS.Aggregates.{
    Torrent,
    Peer
  }
  alias Effusion.CQRS.Commands.{
    AddTorrent,
    AddPeer,
    AddConnectedPeer,
    RemoveConnectedPeer,
    StartDownload,
    StopDownload,
    StoreBlock,
    HandleCompletedDownload,
    HandleBitfield,
    HandleCancel,
    HandleChoke,
    HandleHandshake,
    HandleHave,
    HandleInterested,
    HandlePiece,
    HandleRequest,
    HandleUnchoke,
    HandleUninterested
  }

  dispatch [
    AddTorrent,
    StartDownload,
    StopDownload,
    StoreBlock,
    HandleCompletedDownload
  ], to: Torrent, identity: :info_hash

  dispatch [
    AddPeer,
    AddConnectedPeer,
    RemoveConnectedPeer,
    HandleBitfield,
    HandleCancel,
    HandleChoke,
    HandleHandshake,
    HandleHave,
    HandleInterested,
    HandlePiece,
    HandleRequest,
    HandleUnchoke,
    HandleUninterested
  ], to: Peer, identity: :internal_peer_id
end
