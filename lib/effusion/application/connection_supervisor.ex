defmodule Effusion.Application.ConnectionSupervisor do
  use DynamicSupervisor

  def start_link([]) do
    DynamicSupervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def start_child(peer) do
    spec = %{
      id: peer.peer_id,
      start: {Effusion.PWP.Connection, :start_link, [peer]}
    }
    DynamicSupervisor.start_child(__MODULE__, spec)
  end
end
