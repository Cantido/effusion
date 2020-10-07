defmodule Effusion.CQRS.EventHandlers.PeerMessenger do
  use Commanded.Event.Handler,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.CQRS.Events.{
    PieceHashSucceeded,
    InterestedSent
  }
  require Logger

  def handle(
    %PieceHashSucceeded{info_hash: info_hash, index: index},
    _metadata
  ) do
    Logger.debug("***** Piece has succeeded, sending :have")
    ConnectionRegistry.btp_broadcast(Effusion.Hash.decode(info_hash), {:have, index})

    :ok
  end

  def handle(
    %InterestedSent{info_hash: info_hash, peer_id: peer_id},
    _metadata
  ) do
    ConnectionRegistry.btp_send(Effusion.Hash.decode(info_hash), peer_id, :interested)

    :ok
  end
end
