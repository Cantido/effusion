defmodule Effusion.IO.PieceQueue do
  use GenServer

  @queue_length 100

  def start_link(_args) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    {:ok, %{queue: [], waiting: []}}
  end

  def push(msg) do
    GenServer.call(__MODULE__, {:push, msg})
  end

  def pop(count) do
    GenServer.call(__MODULE__, {:pop, count})
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
