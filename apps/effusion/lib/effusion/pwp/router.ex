defmodule Effusion.PWP.Router do
  use Commanded.Commands.Router

  alias Effusion.PWP.Peer
  alias Effusion.PWP.Handshake.Commands, as: HandshakeCommands
  alias Effusion.PWP.Connection.Commands, as: ConnectionCommands
  alias Effusion.PWP.Messages.Incoming.Commands, as: IncomingMessageCommands
  alias Effusion.PWP.Messages.Outgoing.Commands, as: OutgoingMessageCommands
  alias Effusion.PWP.Swarm.Commands, as: SwarmCommands

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
    OutgoingMessageCommands.CancelRequest,
    OutgoingMessageCommands.RequestBlock,
    OutgoingMessageCommands.SendBitfield,
    OutgoingMessageCommands.SendHave,
    OutgoingMessageCommands.SendInterested,
    SwarmCommands.AddPeerAddress
  ], to: Peer, identity: :peer_uuid
end
