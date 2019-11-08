defmodule Effusion.Application.DownloadServerSupervisor do
  use DynamicSupervisor

  @moduledoc """
  Supervises a dynamic number of `Effusion.BTP.DownloadServer` processes.
  """

  def start_link([]) do
    DynamicSupervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def start_child(args) do
    DynamicSupervisor.start_child(__MODULE__, {Effusion.BTP.DownloadServer, args})
  end
end
