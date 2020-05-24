defmodule Effusion.IO.PieceWriter do
  use GenStage

  def start_link(_opts) do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    {:consumer, 0, [subscribe_to: [Effusion.BTP.VerifiedPieceProducer]]}
  end

  @impl true
  def handle_events(events, _from, state) do
    Enum.each(events, &handle_event/1)
    {:noreply, [], state}
  end

  defp handle_event(event) do
    Effusion.IO.write_piece(event)
    Effusion.BTP.Pieces.mark_piece_written(event.info_hash, event.index)
  end
end
