defmodule Effusion.Pipeline.PieceVerifier do
  use GenStage
  alias Effusion.BTP.Piece
  alias Effusion.Pipeline.MessageConsumer
  alias GenStage.BroadcastDispatcher
  require Logger

  @moduledoc """
  Checks incoming blocks to see if they finish any pieces of their torrent.
  """

  def start_link(_opts) do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    {:producer_consumer, 0, [subscribe_to: [MessageConsumer], dispatcher: BroadcastDispatcher]}
  end

  @impl true
  def handle_events(events, _from, state) do
    verified_pieces =
      events
      |> Enum.map(&Piece.verify/1)
      |> Enum.reject(&is_nil/1)

    {:noreply, verified_pieces, state}
  end
end
