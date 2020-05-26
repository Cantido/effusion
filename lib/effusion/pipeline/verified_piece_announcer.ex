defmodule Effusion.Pipeline.VerifiedPieceAnnouncer do
  use GenStage

  def start_link(_opts) do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    {:consumer, 0, [subscribe_to: [Effusion.Pipeline.VerifiedPieceProducer]]}
  end

  @impl true
  def handle_events(events, _from, state) do
    Enum.each(events, &handle_event/1)
    {:noreply, [], state}
  end

  defp handle_event(piece) do
    Effusion.BTP.ProtocolHandler.have_piece(piece.info_hash, piece)
  end
end
