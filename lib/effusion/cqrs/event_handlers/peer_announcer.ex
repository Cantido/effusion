defmodule Effusion.CQRS.EventHandlers.PeerAnnouncer do
  use Commanded.Event.Handler,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  alias Effusion.CQRS.Events.{
    BlockStored,
    PieceHashSucceeded,
    AllPiecesVerified
  }
  alias Effusion.BTP.Piece
  alias Effusion.IO
  alias Effusion.Repo
  import Ecto.Query
  require Logger

  def handle(
    %PieceHashSucceeded{info_hash: info_hash, index: index},
    _metadata
  ) do
    info_hash = Effusion.Hash.decode(info_hash)

    Effusion.BTP.Pieces.mark_piece_written(info_hash, index)

    :ok
  end

  # def handle(
  #   %AllPiecesVerified{info_hash: info_hash},
  #   _metadata
  # ) do
  #   decoded_info_hash = Effusion.Hash.decode(info_hash)
  #   Logger.debug("****** CQRS: All pieces are written, notifying BTP handler")
  #   Effusion.BTP.ProtocolHandler.notify_all_pieces_written(decoded_info_hash)
  #
  #   :ok
  # end
end
