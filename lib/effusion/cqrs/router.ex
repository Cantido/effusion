defmodule Effusion.CQRS.Router do
  use Commanded.Commands.Router
  alias Effusion.CQRS.Aggregates.{
    Torrent,
    Peer
  }
  alias Effusion.CQRS.Commands.{
    AddTorrent,
    AddPeer,
    PauseDownload,
    StartDownload,
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
    PauseDownload,
    StartDownload,
    StoreBlock,
    HandleCompletedDownload
  ], to: Torrent, identity: :info_hash

  dispatch [
    AddPeer,
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
  ], to: Peer, identity: :peer_id
end
