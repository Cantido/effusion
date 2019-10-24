defmodule Effusion.Application.ConnectionSupervisor do
  use DynamicSupervisor

  def start_link([]) do
    DynamicSupervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def start_child(peer) when is_map(peer) do
    spec = {Effusion.PWP.Connection, peer}
    DynamicSupervisor.start_child(__MODULE__, spec)
  end
end
