defmodule Effusion.Pipeline.PieceBroadway do
  use Broadway
  alias Effusion.BTP.Piece
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.ProtocolHandler
  alias Effusion.BTP.Torrent
  alias Effusion.IO
  alias Effusion.Repo
  alias Broadway.Message
  require Logger

  def start_link(_opts) do
    Broadway.start_link(__MODULE__,
      name: __MODULE__,
      producer: [
        module: {Queutils.BlockingQueueProducer, queue: PieceQueue},
        transformer: {Effusion.Pipeline.Transformer, :transform, []}
      ],
      processors: [
        default: []
      ]
    )
  end

  def handle_message(_, %Message{data:  piece} = message, _) do
    piece = Effusion.Repo.preload(piece, [:torrent])
    ProtocolHandler.have_piece(piece.torrent.info_hash, piece)

    info_hash = piece.torrent.info_hash
    index = piece.index

    IO.write_piece(piece)
    Pieces.mark_piece_written(info_hash, index)

    if Pieces.all_written?(info_hash) && !Torrent.finished?(info_hash) do
      Logger.debug("All pieces are written, notifying BTP handler")
      ProtocolHandler.notify_all_pieces_written(info_hash)
    end
    message
  end
end
