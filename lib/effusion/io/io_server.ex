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
    message = %Message{
      data: {info_hash, block},
      acknowledger: {__MODULE__, :ack_id, :ack_data}
    }
    {:noreply, [message], []}
  end

  def handle_demand(_, _) do
    {:noreply, [], []}
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
