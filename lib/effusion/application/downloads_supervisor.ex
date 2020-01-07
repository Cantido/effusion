defmodule Effusion.Application.DownloadsSupervisor do
  use DynamicSupervisor

  def start_link([]) do
    DynamicSupervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def start_child(info_hash) do
    DynamicSupervisor.start_child(__MODULE__, {Effusion.Application.DownloadSupervisor, info_hash})
  end
end
