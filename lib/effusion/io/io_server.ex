defmodule Effusion.IO.Server do
  use GenServer
  require Logger

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

  def handle_cast({:write, info_hash, block}, []) do
    Effusion.IO.write_piece(info_hash, block)
    |> Enum.reject(fn {_path, result} -> result == :ok end)
    |> Enum.each(fn {path, {:error, reason}} ->
      Logger.error("Error writing file #{path}: #{inspect reason}")
    end)
    {:noreply, []}
  end
end
