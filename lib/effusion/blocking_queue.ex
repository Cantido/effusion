defmodule Effusion.BlockingQueue do
  use GenServer

  @queue_length 100

  def start_link(opts) do
    name = Keyword.get(opts, :name)
    GenServer.start_link(__MODULE__, :ok, name: name)
  end

  def child_spec(opts) do
    %{
      id: Keyword.get(opts, :name, BlockingQueue),
      start: {__MODULE__, :start_link, [opts]},
      type: :supervisor
    }
  end

  def init(:ok) do
    {:ok, %{queue: [], waiting: []}}
  end

  def push(queue, msg) do
    GenServer.call(queue, {:push, msg})
  end

  def pop(queue, count) do
    GenServer.call(queue, {:pop, count})
  end

  def handle_call({:push, msg}, from, state) do
    if Enum.count(state.queue) >= @queue_length do
      waiting = state.waiting ++ [{from, msg}]
      {:noreply, %{state | waiting: waiting}}
    else
      queue = state.queue ++ [msg]
      {:reply, :ok, %{state | queue: queue}}
    end
  end

  def handle_call({:pop, count}, _from, state) do
    {popped, remaining} = Enum.split(state.queue, count)
    {popped_waiters, still_waiting} = Enum.split(state.waiting, count)

    msgs_from_waiters = Enum.map(popped_waiters, fn {from, msg} ->
      GenServer.reply(from, :ok)
      msg
    end)

    queue = remaining ++ msgs_from_waiters

    {:reply, popped, %{state | queue: queue, waiting: still_waiting}}
  end
end
