defmodule Effusion.IOStage do
  require Logger
  use GenStage

  def start_link(_) do
    GenStage.start_link(__MODULE__, :ok)
  end

  def init(:ok) do
    {:consumer, :the_state_does_not_matter, [subscribe_to: [Effusion.PieceStage]]}
  end

  def handle_events(events, _from, state) do
    Enum.map(events, &handle_event/1)
    {:noreply, [], state}
  end

  defp handle_event({info, file, piece}) do
    Effusion.IO.write_piece(info, file, piece)
  end
end
