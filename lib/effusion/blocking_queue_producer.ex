defmodule Effusion.BlockingQueueProducer do
  use GenStage
  require Logger

  def start_link(opts) do
    name = Keyword.get(opts, :name, BlockingQueueProducer)
    GenStage.start_link(__MODULE__, opts, name: name)
  end

  def child_spec(opts) do
    %{
      id: Keyword.get(opts, :name, BlockingQueueProducer),
      start: {__MODULE__, :start_link, [opts]},
      type: :supervisor
    }
  end

  @impl true
  def init(opts) do
    poll_interval = Keyword.get(opts, :poll_interval, 250)
    dispatcher = Keyword.get(opts, :dispatcher, GenStage.DemandDispatcher)
    queue = Keyword.get(opts, :queue, BlockingQueue)
    Process.send_after(self(), :poll, poll_interval)
    {:producer, %{queue: queue, demand: 0, poll_interval: poll_interval}, dispatcher: dispatcher}
  end

  @impl true
  def handle_info(:poll, state) do
    events = Effusion.BlockingQueue.pop(state.queue, state.demand)
    remaining_demand = state.demand - Enum.count(events)

    Process.send_after(self(), :poll, state.poll_interval)
    {:noreply, events, %{state | demand: remaining_demand}}
  end

  @impl true
  def handle_demand(demand, state) do
    total_demand = demand + state.demand
    events = Effusion.BlockingQueue.pop(state.queue, total_demand)
    remaining_demand = total_demand - Enum.count(events)
    {:noreply, events, %{state | demand: remaining_demand}}
  end
end
