defmodule Effusion.IO.Server do
  use GenStage
  alias Effusion.BTP.Pieces
  alias Broadway.Message
  require Logger

  @behaviour Broadway.Acknowledger

  @moduledoc """
  A process that performs IO.
  """

  def start_link(_opts) do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    Process.send_after(self(), :poll, 250)
    {:producer, 0}
  end

  @impl true
  def handle_info(:poll, demand) do
    events = pop_from_queue(demand)
    remaining_demand = demand - Enum.count(events)

    Process.send_after(self(), :poll, 250)
    {:noreply, events, remaining_demand}
  end

  @impl true
  def handle_demand(demand, existing_demand) do
    total_demand = demand + existing_demand
    events = pop_from_queue(total_demand)
    remaining_demand = total_demand - Enum.count(events)
    {:noreply, events, remaining_demand}
  end

  defp pop_from_queue(demand) do
    Effusion.IO.PieceQueue.pop(demand) |> Enum.map(&transform/1)
  end

  defp transform(piece) do
    %Message{
      data: piece,
      acknowledger: {__MODULE__, :ack_id, :ack_data}
    }
  end

  @impl true
  def configure(_, _, _) do
    {:ok, nil}
  end

  @impl true
  def ack(_ack_ref, successful, _failed) do
    Enum.each(successful, fn %Message{data: {info_hash, %{index: index}}} ->
      Pieces.mark_piece_written(info_hash, index)
    end)
  end
end
