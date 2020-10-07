defmodule Effusion.Pipeline.BlockBroadway do
  use Broadway
  alias Effusion.CQRS.Contexts.Downloads
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

  def handle_message(_, %Message{data:  {info_hash, from, {:piece, block}}} = message, _) do
    Downloads.store_block(info_hash, from, block.index, block.offset, block.data)

    message
  end
end
