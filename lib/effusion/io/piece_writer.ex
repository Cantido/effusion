defmodule Effusion.IO.PieceWriter do
  use GenStage

  def start_link(_opts) do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    {:consumer, 0, [subscribe_to: [PieceProducer]]}
  end

  @doc """
  Consume a GenStage event to write a piece.
  """
  @impl true
  def handle_events(events, _from, state) do
    Enum.each(events, &handle_event/1)
    {:noreply, [], state}
  end

  defp handle_event(event) do
    Effusion.IO.write_piece(event)
    {info_hash, %{index: index}} = event
    Effusion.BTP.Pieces.mark_piece_written(info_hash, index)
  end
end
