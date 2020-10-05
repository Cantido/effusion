defmodule Effusion.Pipeline.BlockBroadway do
  use Broadway
  alias Effusion.CQRS.Contexts.Pieces, as: PiecesContext
  alias Effusion.BTP.Piece
  alias Effusion.Repo
  alias Broadway.Message

  def start_link(_opts) do
    Broadway.start_link(__MODULE__,
      name: __MODULE__,
      producer: [
        module: {Queutils.BlockingQueueProducer, queue: BlockQueue},
        transformer: {Effusion.Pipeline.Transformer, :transform, []}
      ],
      processors: [
        default: []
      ]
    )
  end

  def handle_message(_, %Message{data:  {info_hash, _from, {:piece, block}}} = message, _) do
    piece =
      Piece.get(info_hash, block.index)
      |> Repo.one()
      |> Piece.verify()

    unless is_nil(piece) do
      Queutils.BlockingQueue.push(PieceQueue, piece)
    end

    message
  end
end
