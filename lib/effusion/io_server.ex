defmodule Effusion.IOServer do
  use GenServer

  @moduledoc """
  A process that performs IO.
  """

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @doc """
  Pull the given piece out of the database and write it out to the configured file.
  """
  def write_piece(info, block) do
    GenServer.cast(__MODULE__, {:write, info, block})
  end

  def init(:ok) do
    {:ok, []}
  end

  def handle_cast({:write, info_hash, block}, state) do
    :ok = Effusion.IO.write_piece(info_hash, block)
    {:noreply, state}
  end
end
