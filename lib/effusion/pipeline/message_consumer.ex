defmodule Effusion.Pipeline.MessageConsumer do
  use GenStage
  require Logger

  def start_link(_opts) do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    {:producer_consumer, 0, [subscribe_to: [MessageProducer]]}
  end

  @impl true
  def handle_events(events, _from, state) do
    updated_pieces = Enum.map(events, fn event ->
      Effusion.PWP.ProtocolHandler.handle_message(event)

      case event do
        {info_hash, _from, {:piece, block}} ->
          Effusion.BTP.Piece.get(info_hash, block.index)
          |> Effusion.Repo.one()
        _ -> nil
      end
    end)
    |> Enum.reject(&is_nil/1)
    {:noreply, updated_pieces, state}
  end
end
