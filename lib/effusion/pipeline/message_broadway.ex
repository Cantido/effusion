defmodule Effusion.Pipeline.MessageBroadway do
  use Broadway
  alias Effusion.PWP.ProtocolHandler
  alias Broadway.Message

  def start_link(_opts) do
    Broadway.start_link(__MODULE__,
      name: __MODULE__,
      producer: [
        module: {Queutils.BlockingQueueProducer, queue: MessageQueue},
        transformer: {Effusion.Pipeline.Transformer, :transform, []}
      ],
      processors: [
        default: []
      ]
    )
  end

  def handle_message(_, %Message{data: data} = message, _) do
    ProtocolHandler.handle_message(data)

    case data do
      {_info_hash, _from, {:piece, _block}} -> Queutils.BlockingQueue.push(BlockQueue, data)
      _ -> nil
    end

    message
  end
end
