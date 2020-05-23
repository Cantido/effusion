defmodule Effusion.IO.PieceQueue do
  use GenServer

  def start_link(_args) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    {:ok, []}
  end

  def push(msg) do
    GenServer.call(__MODULE__, {:push, msg})
  end

  def pop(count) do
    GenServer.call(__MODULE__, {:pop, count})
  end

  def handle_call({:push, msg}, _from, state) do
    {:reply, :ok, [msg | state]}
  end

  def handle_call({:pop, count}, _from, state) do
    {popped, remaining} = Enum.split(state, count)

    {:reply, popped, remaining}
  end
end
