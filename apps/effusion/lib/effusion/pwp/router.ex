defmodule Effusion.PWP.Router do
  use Commanded.Commands.Router

  alias Effusion.PWP.Peer
  alias Effusion.PWP.Commands.{
    Connection,
    Handshake,
    Incoming,
    Outgoing,
    Swarm,
  }

  dispatch [
    Connection.AddConnectedPeer,
    Connection.AddOpenedPeerConnection,
    Connection.AttemptToConnect,
    Connection.DisconnectPeer,
    Connection.HandleFailedConnectionAttempt,
    Connection.RemoveConnectedPeer,
    Handshake.HandleHandshake,
    Handshake.SendHandshake,
    Handshake.TimeoutHandshake,
    Incoming.HandleBitfield,
    Incoming.HandleCancel,
    Incoming.HandleChoke,
    Incoming.HandleHave,
    Incoming.HandleInterested,
    Incoming.HandlePiece,
    Incoming.HandleRequest,
    Incoming.HandleUnchoke,
    Incoming.HandleUninterested,
    Outgoing.CancelRequest,
    Outgoing.RequestBlock,
    Outgoing.SendBitfield,
    Outgoing.SendHave,
    Outgoing.SendInterested,
    Swarm.AddPeerAddress
  ], to: Peer, identity: :peer_uuid
end
