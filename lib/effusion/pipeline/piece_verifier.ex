defmodule Effusion.Pipeline.PieceVerifier do
  use GenStage
  alias Effusion.BTP.Piece
  require Logger

  def start_link(_opts) do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    {:producer_consumer, 0, [subscribe_to: [Effusion.Pipeline.MessageConsumer], dispatcher: GenStage.BroadcastDispatcher]}
  end

  @impl true
  def handle_events(events, _from, state) do
    verified_pieces =
      Enum.map(events, &handle_event/1)
      |> Enum.reject(&is_nil/1)

    {:noreply, verified_pieces, state}
  end

  def handle_event(piece = %Piece{}) do
    Logger.debug("Verifying piece #{piece.index}")
    Effusion.BTP.Piece.has_all_blocks?(piece)
    |> Effusion.Repo.one()
    |> Effusion.BTP.Piece.verify()
  end
end
