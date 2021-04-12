defmodule Effusion.PWP.Router do
  use Commanded.Commands.Router

  alias Effusion.PWP.Peer
  alias Effusion.PWP.Handshake.Commands, as: HandshakeCommands
  alias Effusion.PWP.Connection.Commands, as: ConnectionCommands
  alias Effusion.PWP.Messages.Incoming.Commands, as: IncomingMessageCommands
  alias Effusion.PWP.Commands.{
    Outgoing,
    Swarm,
  }

  dispatch [
    ConnectionCommands.AddConnectedPeer,
    ConnectionCommands.AddOpenedPeerConnection,
    ConnectionCommands.AttemptToConnect,
    ConnectionCommands.DisconnectPeer,
    ConnectionCommands.HandleFailedConnectionAttempt,
    ConnectionCommands.RemoveConnectedPeer,
    HandshakeCommands.HandleHandshake,
    HandshakeCommands.SendHandshake,
    HandshakeCommands.TimeoutHandshake,
    IncomingMessageCommands.HandleBitfield,
    IncomingMessageCommands.HandleCancel,
    IncomingMessageCommands.HandleChoke,
    IncomingMessageCommands.HandleHave,
    IncomingMessageCommands.HandleInterested,
    IncomingMessageCommands.HandlePiece,
    IncomingMessageCommands.HandleRequest,
    IncomingMessageCommands.HandleUnchoke,
    IncomingMessageCommands.HandleUninterested,
    Outgoing.CancelRequest,
    Outgoing.RequestBlock,
    Outgoing.SendBitfield,
    Outgoing.SendHave,
    Outgoing.SendInterested,
    Swarm.AddPeerAddress
  ], to: Peer, identity: :peer_uuid
end
