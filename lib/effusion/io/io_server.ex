defmodule Effusion.IO.Server do
  use GenStage
  require Logger

  @moduledoc """
  A process that performs IO.
  """

  def start_link(_opts) do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @doc """
  Pull the given piece out of the database and write it out to the configured file.
  """
  def write_piece(info, block) do
    GenStage.cast(__MODULE__, {:write, info, block})
  end

  def init(:ok) do
    {:producer, []}
  end

  def handle_cast({:write, info_hash, block}, []) do
    {:noreply, [{info_hash, block}], []}
  end

  def handle_demand(_, _) do
    {:noreply, [], []}
  end
end
