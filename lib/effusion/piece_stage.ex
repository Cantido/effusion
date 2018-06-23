defmodule Effusion.PieceStage do
  use GenStage

  def start_link(_) do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def put_event(event) do
    GenStage.call(__MODULE__, {:put_event, event})
  end

  def init(:ok) do
    {:producer, []}
  end

  def handle_call({:put_event, event}, _from, state) do
    {:reply, :ok, [event], state}
  end

  def handle_demand(_, state)  do
    {:noreply, [], state}
  end
end
