defmodule Effusion.PeerConnectionSupervisor do
  use Supervisor

  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def start_child(client) do
    Task.Supervisor.start_child(Effusion.TaskSupervisor, fn -> Effusion.PeerConnection.serve(client) end)
  end

  def init(:ok) do
    Supervisor.init([Task], strategy: :simple_one_for_one)
  end
end
