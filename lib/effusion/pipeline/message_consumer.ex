defmodule Effusion.Pipeline.MessageConsumer do
  use GenStage
  alias Effusion.BTP.Piece
  alias Effusion.PWP.ProtocolHandler
  alias Effusion.Repo
  require Logger

  @moduledoc """
  Handles messages that come in from peers.
  """

  def start_link(_opts) do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    {:producer_consumer, 0, [subscribe_to: [MessageProducer]]}
  end

  @impl true
  def handle_events(events, _from, state) do
    updated_pieces =
      events
      |> Enum.map(&handle_event/1)
      |> Enum.reject(&is_nil/1)

    {:noreply, updated_pieces, state}
  end

  defp handle_event(event) do
    ProtocolHandler.handle_message(event)

    case event do
      {info_hash, _from, {:piece, block}} ->
        Piece.get(info_hash, block.index)
        |> Repo.one()
      _ -> nil
    end
  end
end
