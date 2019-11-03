defmodule Effusion.IOServer do
  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def write_piece(info, destdir, block) do
    GenServer.cast(__MODULE__, {:write, info, destdir, block})
  end

  def init(:ok) do
    {:ok, []}
  end

  def handle_cast({:write, info, destdir, block}, state) do
    :ok = Effusion.IO.write_piece(info, destdir, block)
    {:noreply, state}
  end
end
